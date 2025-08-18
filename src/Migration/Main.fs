namespace Aornota.Ubersweep.Migration

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Events

open Serilog

module Main =
    let pathTuples = [
        @"D:\UberSweep\- persisted -\2018-fifa", true
        @"D:\UberSweep\- persisted -\2019-rwc", false
        @"D:\UberSweep\- persisted -\2021-euro", false
        @"D:\UberSweep\- persisted -\2022-fifa", false
        @"D:\UberSweep\- persisted -\2023-rwc", false
        @"D:\UberSweep\- persisted -\2024-euro", false
    ]

    let private checkUsers logger =
        let logger = SourcedLogger.Create("checkUsers", logger)

        let helper = UserHelper() :> IHelper<UserEvent, User>

        let check (path: string, useLegacyDeserializer: bool) =
            logger.Information("Checking Users for {path}...", path)

            match Reader<UserEvent>($"{path}\users", useLegacyDeserializer, logger).ReadAll() with
            | Ok list ->
                logger.Information("...events read without error for {length}", list.Length)

                let mutable okCount = 0

                list
                |> List.iter (fun (guid, events) ->
                    match helper.ApplyEvents(events |> List.map _.Event) with
                    | Ok _ -> okCount <- okCount + 1
                    | Error error -> logger.Error("...error applying events for {guid}: error", guid, error))

                logger.Information("...events applied without error for {okCount}", okCount)
            | Error errors -> logger.Error("...{length} errors when reading events for all", errors.Length)

        pathTuples |> List.iter check

    [<EntryPoint>]
    let main _ =
        Log.Logger <- LoggerConfiguration().MinimumLevel.Debug().WriteTo.Console().CreateLogger()

        checkUsers Log.Logger

        0
