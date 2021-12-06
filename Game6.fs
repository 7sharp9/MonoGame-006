namespace MonoGame006

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open MonoGame.Extended

type AABB =
    { halfExtents: Vector2
      center: Vector2 }

    //calculated properties, these could be calculated in the create methods
    ///aka top left
    member this.min =
        Vector2(this.center.X - this.halfExtents.X, this.center.Y - this.halfExtents.Y)
     ///aka bottom right
    member this.max =
        Vector2(this.center.X + this.halfExtents.X, this.center.Y + this.halfExtents.Y)
    member this.size =
        Vector2(this.halfExtents.X * 2.f, this.halfExtents.Y * 2.f)
    static member create(center, halfExtents) =
        { center = center; halfExtents = halfExtents }

type RigidBody =
    { mass: float32
      inverseMass: float32
      aabb: AABB
      velocity: Vector2
      onGround: bool
      onGroundLast: bool
    }
    static member create (mass, width, height, center, vel) =
        {   mass = mass
            inverseMass = if mass = 0.f then 0.f else 1.f / mass
            aabb = {center = center; halfExtents = Vector2(width/2.f, height/2.f) }
            velocity = vel
            onGround = false
            onGroundLast = false }

type Contact =
    { a: RigidBody
      b: RigidBody
      normal: Vector2
      distance: float32
      impulse: float32 }
    static member create(a, b, normal, dist, ?impulse) =
        {   a = a
            b = b
            normal = normal
            distance = dist
            impulse = Option.defaultValue 0.f impulse }

type Sprite =
    {position: Vector2; speed: float32; texture: Texture2D; size: Point; offset: Point}
    member this.Draw(spriteBatch: SpriteBatch) =
        let sourceRect = Rectangle(this.offset, this.size)
        spriteBatch.Draw(this.texture, this.position, sourceRect, Color.White)

module AnimationFrames =
    let horizontalStrip(frameCount, size: Point, offset: Point) =
        [| for i in 0..frameCount-1 ->
            Rectangle(offset + Point(size.X * i, 0) , size)
        |]

type Animation = 
    {   frames: Rectangle array
        fps: int
        currentFrame: int
        frameTimer: TimeSpan
        frameLength: TimeSpan
        size: Point }

    static member Create(frameCount, fps, size: Point, offset: Point) =
        let frames = AnimationFrames.horizontalStrip(frameCount, size, offset)
        {   frames = frames
            currentFrame = 0
            frameTimer = TimeSpan.Zero
            frameLength = TimeSpan.FromSeconds (float (1.f / float32 fps))
            fps = fps
            size = size }
    
    member this.CurrentFrame =
        this.frames[this.currentFrame]

module Animation =
    let reset anim =
        {anim with
            currentFrame = 0
            frameTimer = TimeSpan.Zero }
        
    let update (gameTime: GameTime) (anim: Animation) =
        let newframeTimer, newFrame =
            match anim.frameTimer + gameTime.ElapsedGameTime with
            | n when n >= anim.frameLength ->
                TimeSpan.Zero, (anim.currentFrame + 1) % anim.frames.Length
            | n -> n, anim.currentFrame

        {anim with
            frameTimer = newframeTimer
            currentFrame = newFrame }

type AnimationKey =
    | IdleLeft
    | IdleRight
    | WalkLeft
    | WalkRight

type AnimatedSprite =
    {   texture: Texture2D
        animations: Map<AnimationKey, Animation>
        currentAnimationKey: AnimationKey
        isAnimating: bool
        speed: float32
        jump: float32
        rigidBody: RigidBody }
    member this.CurrentAnimation = this.animations[this.currentAnimationKey]
    member this.Size with get() = this.CurrentAnimation.size

module AnimatedSprite =  

    let resetAnimation key animatedSprite =
        animatedSprite.animations[key]
        |> Animation.reset
        
    let updateAnimation key gameTime animatedSprite =
        let animation = animatedSprite.animations[key]
        if animatedSprite.isAnimating then
            animation |> Animation.update gameTime
        else animation

    let draw (animSprite: AnimatedSprite) (gameTime: GameTime) (sb: SpriteBatch) =
        sb.Draw(animSprite.texture, animSprite.rigidBody.aabb.min - Vector2(8.f, 13.f), animSprite.CurrentAnimation.CurrentFrame, Color.White)
        
[<AutoOpen>]
module MonoGameExtensions =
    type Viewport with
        member this.Center =
            Vector2(float32 this.Width * 0.5f, float32 this.Height * 0.5f)

    type Vector2 with
        member this.MajorAxis() =
            if abs this.X > abs this.Y then
                Vector2(float32 (sign this.X), 0.f)
            else
                Vector2(0.f, float32 (sign this.Y))

type Camera(viewport: Viewport) =       
    member val WorldToScreen = Matrix.Identity with get, set
    member val ScreenToWorld = Matrix.Identity with get, set
    member val Zoom = 1.0f with get, set
    member val Position = Vector2.Zero with get, set
    member val Rotation = 0.0f with get, set

    member this.Update (pos:Vector2) =
        this.Position <- pos
        this.WorldToScreen <-
            Matrix.CreateTranslation(Vector3(-pos, 0.0f)) *
            Matrix.CreateRotationZ(this.Rotation ) *
            Matrix.CreateScale(Vector3(this.Zoom, this.Zoom, 1.f )) *
            Matrix.CreateTranslation(Vector3(viewport.Center, 0.f))
        this.ScreenToWorld <- Matrix.Invert(this.WorldToScreen)

type TileSet =
    { tilesWide: int
      tilesHigh: int
      tileWidth: int
      tileHeight: int
      texture:  Texture2D
      sourceRectangles: Rectangle array }

module TileSet =
    let createTileSet(tileswide, tileshigh,  tileheight, tilewidth, texture) =
      let sourceRectangles =
           [| for y in 0..tileshigh-1 do
                for x in 0..tileswide-1 do
                    Rectangle(x * tilewidth, y * tileheight, tilewidth, tileheight) |]

      { tilesWide = tileswide
        tilesHigh = tileshigh
        tileWidth = tilewidth
        tileHeight = tileheight
        texture = texture
        sourceRectangles = sourceRectangles }

    let tileToWorld x y (tileSet: TileSet) =
        Vector2(float32 (x * tileSet.tileWidth), float32 (y * tileSet.tileHeight))

    let toAABB x y (tileSet: TileSet) =
        let tileInWorld = tileToWorld x y tileSet
       
        let extents = Vector2(float32 tileSet.tileWidth / 2.0f, float32  tileSet.tileHeight / 2.0f)
        AABB.create(tileInWorld + extents, extents)

type TileLayer = { tiles: int array
                   width: int
                   height: int
                   visible: bool }

module TileLayer =

    let getTileId x y (layer: TileLayer) =
        match x, y with
        | (x, y) when x < 0 || y < 0 -> None
        | (x ,y) when x > layer.width || y > layer.height ->
            None
        | _ ->
            let index = y * layer.width + x
            match layer.tiles |> Array.tryItem index with
            | Some tileId when tileId > 0 ->
                Some (tileId - 1) //id is one based, zero being an empty cell
            | _ -> None

    let vectorToCell (position: Vector2) (tileSet: TileSet) =
        Point( int position.X / tileSet.tileWidth, int position.Y / tileSet.tileHeight)
            
    let draw(spriteBatch: SpriteBatch, tileSet: TileSet, camera: Camera, layer : TileLayer, game: Game) =
        if not layer.visible then () else
        let cameraPoint =
            let location =
                Vector2(camera.Position.X - (float32 game.GraphicsDevice.Viewport.Width * 0.5f),
                        camera.Position.Y - (float32 game.GraphicsDevice.Viewport.Height * 0.5f))
            vectorToCell location tileSet 

        let viewPoint =
            let location =
                Vector2(camera.Position.X + (float32 game.GraphicsDevice.Viewport.Width * 0.5f),
                        camera.Position.Y + (float32 game.GraphicsDevice.Viewport.Height * 0.5f))
            vectorToCell location tileSet
        
        let minX, minY =  max 0 (cameraPoint.X - 1), max 0 (cameraPoint.Y - 1)
        let maxX, maxY =  min (viewPoint.X + 1) layer.width - 1, min (viewPoint.Y + 1) layer.height - 1
    
        for y in minY..maxY do
            for x in minX..maxX do
                match getTileId x y layer with
                | None  -> ()
                | Some tile ->
                    let destination = Rectangle(x * tileSet.tileWidth, y * tileSet.tileHeight,
                                                tileSet.tileWidth, tileSet.tileHeight)
                    spriteBatch.Draw(tileSet.texture, destination, tileSet.sourceRectangles[tile], Color.White)

    let getBroadphaseTiles (tileLayer: TileLayer) (tileSet: TileSet) (min: Vector2) (max: Vector2) = // action (dt: float32) =

        //round down
        let minX = (int min.X) / tileSet.tileWidth
        let minY = (int min.Y) / tileSet.tileHeight 

        //round up
        let maxX = (int (max.X + 0.5f)) / tileSet.tileWidth
        let maxY = (int (max.Y + 0.5f)) / tileSet.tileHeight

        //get tiles possibly interacting by movable object
        let broadphaseTiles =
            [for x in minX..maxX do
                for y in minY..maxY do
                    let tileaabb = TileSet.toAABB x y tileSet
                    let tileId = getTileId x y tileLayer
                    tileId, tileaabb, x, y]
        broadphaseTiles 


module Speculative =
    let speculativeSolver (dt: float32) (contact: Contact) =

        let normal = -contact.normal

        //get all the relative normal velocity
        let nv = Vector2.Dot(contact.b.velocity - contact.a.velocity, normal)
        if nv > 0.f then
            contact
        else
            //remove enough velocity to leave them just touching
            let remove = nv + (contact.distance / dt)

            //compute impulse
            let impulse = remove / (contact.a.inverseMass + contact.b.inverseMass)

            //accumulate
            let newImpulse = min (impulse + contact.impulse) 0.f

            //compute change
            let change = newImpulse - contact.impulse

            //store accumulated impulse and apply impulse
            { contact with
                a = {contact.a with
                        velocity = contact.a.velocity + change * normal * contact.a.inverseMass}
                b = {contact.b with
                        velocity = contact.b.velocity - change * normal * contact.b.inverseMass}
                impulse = newImpulse }

module Collision =
    let isInternalCollision (tileX: int) (tileY: int) (normal: Vector2) (tileLayer: TileLayer) =
        let nextTileX = tileX + int normal.X
        let nextTileY = tileY + int normal.Y
        
        let currentTile = TileLayer.getTileId tileX tileY tileLayer
        let nextTile    = TileLayer.getTileId nextTileX nextTileY tileLayer
        
        match nextTile with None -> false | Some _ -> true
    
    let AABBVsAABB (a: RigidBody) (b:RigidBody) tileX tileY (map: TileLayer) =
        let combinedExtents = b.aabb.halfExtents + a.aabb.halfExtents
        let delta = b.aabb.center - a.aabb.center

        let normal = delta.MajorAxis() |> Vector2.Negate
        let planeCentre = (normal * combinedExtents) + b.aabb.center
            
        // distance point from plane
        let planeDelta = a.aabb.center - planeCentre
        let dist = planeDelta.Dot normal

        let contact = Contact.create(a, b, normal, dist)

        let internalCollision = isInternalCollision tileX tileY normal map
        {| internalCollision = not internalCollision; contact = contact |}

    let collisionResponse (movableObject: RigidBody) (other: RigidBody) (contact: Contact) (dt: float32)  =
        let friction = 0.4f
        let solved = Speculative.speculativeSolver dt contact 

        let tangent = solved.normal.PerpendicularCounterClockwise()

        let newVelocity =
            //compute the tangent velocity, scale by friction, but only x axis
            if tangent.Y = 0.0f then
                let tv = solved.a.velocity.Dot(tangent) * friction
                solved.a.velocity - (tangent * tv)
            else solved.a.velocity
        newVelocity, solved.normal.Y < 0.f, contact
        
    let innerCollide (tileLayer: TileLayer) (movableObject: RigidBody) (tileAabb: AABB) (tileType: int option) (dt:float32) (x: int) (y: int) =
        match tileType with
        | None -> None
        | tileType ->
            let tileRigidBody = RigidBody.create(0.f, tileAabb.size.X, tileAabb.size.Y, tileAabb.center, Vector2.Zero)
            let collisionResult = AABBVsAABB movableObject tileRigidBody x y tileLayer

            if collisionResult.internalCollision then
                Some (collisionResponse movableObject tileRigidBody collisionResult.contact dt)
            else None

    let collision (map:TileLayer) (tileSet: TileSet) (rigidBody: RigidBody) (dt: float32) =

        let expand = Vector2(5.f, 5.f)
        let predictedPos = rigidBody.aabb.center + (rigidBody.velocity * dt)

        //find min/max
        let mutable min = Vector2.Min(rigidBody.aabb.center, predictedPos)
        let mutable max = Vector2.Max(rigidBody.aabb.center, predictedPos)
        
        //extend by radius
        min <- min - rigidBody.aabb.halfExtents
        max <- max + rigidBody.aabb.halfExtents
        
        //extend more to deal with being close to boundry
        min <- min - expand
        max <- max + expand
        
        TileLayer.getBroadphaseTiles map tileSet min max
        |> List.choose(fun (tileId, tileaabb, x, y) -> innerCollide map rigidBody tileaabb tileId dt x y)

type Game6 () as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this, PreferredBackBufferWidth = 1920, PreferredBackBufferHeight = 1080)
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable playerSpriteSheet = Unchecked.defaultof<Texture2D>
    let mutable player = Unchecked.defaultof<AnimatedSprite>
    let mutable playerAnimations = Unchecked.defaultof<_>
    let mutable camera = Unchecked.defaultof<_>
    let mutable tileSet = Unchecked.defaultof<TileSet>
    let mutable tileLayer = Unchecked.defaultof<TileLayer>
    let mutable terrain = Unchecked.defaultof<Texture2D>
    let gravity = Vector2(0.f, 50.0f)
    let maxSpeed = 350.f

    [<return: Struct>]
    let (|KeyDown|_|) k (state: KeyboardState) =
        if state.IsKeyDown k then ValueSome() else ValueNone

    do
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- false

    override this.Initialize() =
        let frameSize = Point(64, 64)
        let anims = 
            [   IdleLeft,  Animation.Create(1, 1, frameSize, Point(0, 64))
                IdleRight, Animation.Create(1, 1, frameSize, Point(0, 192))
                WalkLeft,  Animation.Create(8, 10, frameSize, Point(64, 64))
                WalkRight, Animation.Create(8, 10, frameSize, Point(64, 192)) ] |> Map.ofList
        playerAnimations <- anims
        camera <- Camera(this.GraphicsDevice.Viewport)
        terrain <- this.Content.Load<Texture2D>("desert")
        tileSet <- TileSet.createTileSet(4, 4, 64, 64, terrain)
        let tiles =
            [|  0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                1;9;9;9;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;9;9;9;9;9;9;9;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;4;3;3;3;3;3;3;3;6;9;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;7;8;8;8;8;8;8;3;3;3;6;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;8;8;8;8;14;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;9;9;9;9;9;9;9;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;1;9;9;9;9;9;9;9;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;11;3;3;3;3;3;3;3;13;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;4;3;3;3;3;3;3;3;13;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;7;8;8;8;8;8;8;8;8;14;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;7;8;8;8;8;8;8;8;6;10;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;4;3;13;0;0;0;0;0;0;0;0;0;0;0;1;9;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;7;8;14;0;0;0;0;0;0;0;0;0;0;0;7;8;14;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;9;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;7;8;14;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;1;9;9;9;9;5;0;0;0;0;0;0;0;0;0;0;1;9;9;9;9;9;9;9;9;9;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;1;9;5;0;0;4;3;3;3;3;6;9;5;0;0;0;0;0;0;0;0;4;3;3;3;3;3;3;3;3;3;6;5;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;4;3;13;0;0;7;8;8;3;3;3;3;13;0;0;0;0;0;0;0;0;7;8;8;8;8;8;8;8;3;3;3;13;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;7;8;14;0;0;0;0;0;7;8;8;8;14;0;0;1;9;5;0;0;0;0;0;0;0;0;0;0;0;7;8;8;14;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0
                9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9;9
        |]

        tileLayer <- { tiles = tiles
                       width = 60
                       height = 28
                       visible = true }
        base.Initialize()

    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        playerSpriteSheet <- this.Content.Load<Texture2D>("skeleton")

        let body = RigidBody.create(60.f, 48.f, 48.f, Vector2.Zero, Vector2.Zero)
        player <- {  texture = playerSpriteSheet
                     animations = playerAnimations
                     currentAnimationKey = IdleRight
                     isAnimating = false
                     speed = 166.f
                     jump = 600.f
                     rigidBody = body }

    override this.Update (gameTime) =
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
        then this.Exit()

        let dt = float32 gameTime.ElapsedGameTime.TotalSeconds

        let walkingToIdle = function
            | WalkLeft -> IdleLeft
            | WalkRight -> IdleRight
            | otherIdle -> otherIdle

        let movementVelocity, isMoving, animationKey =
            let movementVelocity, animationKey =
                let xVelocity, yVelocity =
                    if player.rigidBody.onGround then
                        player.speed, -player.jump
                    else player.speed / 10.f, 0.f
                match Keyboard.GetState() with
                | KeyDown Keys.W & KeyDown Keys.A ->
                    Vector2(-xVelocity, yVelocity), IdleLeft
                | KeyDown Keys.W & KeyDown Keys.D ->
                    Vector2(xVelocity, yVelocity), IdleRight
                | KeyDown Keys.W ->
                    Vector2(0.f, yVelocity), player.currentAnimationKey
                | KeyDown Keys.A ->
                    Vector2(-xVelocity, 0.f), WalkLeft
                | KeyDown Keys.D ->
                    Vector2(xVelocity, 0.f), WalkRight
                | _ ->
                    Vector2.Zero, player.currentAnimationKey
            
            let isMoving = movementVelocity <> Vector2.Zero

            let animation =
                if player.rigidBody.onGround then 
                    if isMoving then animationKey
                    else walkingToIdle animationKey 
                else
                    if isMoving then walkingToIdle animationKey 
                    else walkingToIdle player.currentAnimationKey

            movementVelocity, isMoving, animation

        let newAnimation =
            if player.currentAnimationKey = animationKey then
                player |> AnimatedSprite.updateAnimation animationKey gameTime
            else
                player |> AnimatedSprite.resetAnimation animationKey

        //update players volocity due to movement and gravity
        let movementAndGravityVelocity =
            let velocity = player.rigidBody.velocity + movementVelocity + gravity
            //clamp max speed to ±maxspeed for x, ±maxspeed*2 for y
            Vector2.Clamp(velocity, Vector2(-maxSpeed, -maxSpeed * 4.f), Vector2(maxSpeed, maxSpeed * 2.f))

        let rbUpdated = {player.rigidBody with
                            onGround = false
                            onGroundLast = player.rigidBody.onGround
                            velocity = movementAndGravityVelocity }

        //do world collision between player and map
        let afterCollisionVelocity, onGround, onGroundLast =
            Collision.collision tileLayer tileSet rbUpdated dt
            |> List.sortBy (fun (_,_,c) -> c.distance)
            |> List.tryHead
            |> function
               | None ->
                    rbUpdated.velocity, rbUpdated.onGround, rbUpdated.onGroundLast
               | Some (velocity, onGround, contact) ->
                    velocity, onGround, rbUpdated.onGroundLast

        //integrate velocity into position
        let newPosition =
            let maxClamp =
                Vector2(float32 (tileLayer.width * tileSet.tileWidth) - float32 player.rigidBody.aabb.halfExtents.X,
                        float32 (tileLayer.height * tileSet.tileHeight) - float32 player.rigidBody.aabb.halfExtents.Y)

            let pos = player.rigidBody.aabb.center + (afterCollisionVelocity * dt)
            Vector2.Clamp(pos, player.rigidBody.aabb.halfExtents, maxClamp)

        player <- { player with
                        rigidBody =
                            {player.rigidBody with
                                velocity = afterCollisionVelocity
                                aabb = { player.rigidBody.aabb with center = newPosition }
                                onGround = onGround
                                onGroundLast = onGroundLast }
                        isAnimating = isMoving
                        currentAnimationKey = animationKey
                        animations = player.animations |> Map.add animationKey newAnimation }

        camera.Update player.rigidBody.aabb.center

        base.Update(gameTime)

    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.CornflowerBlue
        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.AlphaBlend, SamplerState.PointClamp, transformMatrix = camera.WorldToScreen)
        TileLayer.draw(spriteBatch, tileSet, camera, tileLayer, this)
        AnimatedSprite.draw player gameTime spriteBatch
        //let playeraabb = Rectangle(player.rigidBody.aabb.min.ToPoint(), player.rigidBody.aabb.size.ToPoint()).ToRectangleF()
        //spriteBatch.DrawRectangle(playeraabb, Color.Blue)
        spriteBatch.End()
        base.Draw(gameTime)