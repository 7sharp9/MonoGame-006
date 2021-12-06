namespace MonoGame006

module Program =

    open System
    open Microsoft.Xna.Framework

    [<EntryPoint>]
    let main argv =
        use game = new Game6()
        game.Run()
        0 // return an integer exit code
