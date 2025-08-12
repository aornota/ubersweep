namespace Aornota.Ubersweep.Tests.Server

open Aornota.Ubersweep.Tests.Server.Entities
open Aornota.Ubersweep.Tests.Server.Persistence

open Expecto

module Main =
    let tests =
        testList "Server" [
            EntityEventHelperTests.tests
            FileReaderAndWriterTests.tests
            UserTests.tests
        ]

    [<EntryPoint>]
    let main _ = runTestsWithCLIArgs [] [||] tests
