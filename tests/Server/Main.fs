namespace Aornota.Ubersweep.Tests.Server

open Aornota.Ubersweep.Tests.Server.Entities
open Aornota.Ubersweep.Tests.Server.Persistence

open Expecto
open Serilog

module Main =
    let tests =
        testList "Server" [
            EntityEventHelperTests.tests
            FileReaderAndWriterTests.tests
            UserTests.tests
        ]

    [<EntryPoint>]
    let main _ =
        (* TODO: Decide if logging is useful - and how best to configure it if so...
        Log.Logger <-
            LoggerConfiguration()
                .MinimumLevel.Information()
                .WriteTo.Console()
                .CreateLogger()
        *)

        runTestsWithCLIArgs [] [||] tests
