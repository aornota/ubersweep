namespace Aornota.Ubersweep.Tests.Server

open Aornota.Ubersweep.Tests.Server.Entities
open Aornota.Ubersweep.Tests.Server.Persistence

open Expecto
open Serilog
open Serilog.Sinks.SystemConsole.Themes

module Main =
    let tests =
        testList "Server" [
            EntityHelperTests.tests
            UserTests.tests
            FilePersistenceTests.tests
            FilePersistenceFactoryTests.tests
        ]

    [<EntryPoint>]
    let main _ =
        (* TODO-TESTS: Decide if logging is useful - and how best to configure it (e.g. currently not showing timestamp or colous) if so...
        Log.Logger <-
            LoggerConfiguration()
                .MinimumLevel.Information()
                .WriteTo.Console(
                    theme = SystemConsoleTheme.Literate,
                    outputTemplate = "[{Timestamp:HH:mm:ss.fff zzz} {Level:u3}] {SourceContext} {Message:lj}{NewLine}"
                )
                .CreateLogger()
        *)

        runTestsWithCLIArgs [] [||] tests
