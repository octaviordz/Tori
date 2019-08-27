module Program =
    [<EntryPoint>]
    let main _ =
        // Wait for user input to prevent application termination.
        System.Console.ReadLine() |> ignore
        let test = ClientServerTests.ClientServer(null)
        test.``Multiple clients single message per client``()
        0
