namespace Aornota.Ubersweep.Tests.Server

open Expecto

module Main =
    let tests =
        testList "Server" [ EntityEventHelperTests.tests; FileReaderAndWriterTests.tests ]

    [<EntryPoint>]
    let main _ = runTestsWithCLIArgs [] [||] tests
