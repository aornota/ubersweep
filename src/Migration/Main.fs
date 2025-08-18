namespace Aornota.Ubersweep.Migration

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Migration.Events

open Serilog
open System.IO

type private Checker<'event, 'entity>(subPath: string, helper: IHelper<'event, 'entity>, logger: ILogger) =
    let logger = SourcedLogger.Create(typeof<'entity>.Name, logger)

    let check (path: string, useLegacyDeserializer: bool) =
        let path = Path.Combine(path, subPath)

        logger.Information(@"Checking {path}...", path)

        match Reader<'event>($"{path}", useLegacyDeserializer, logger).ReadAll() with
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

    member _.Check pathTuples = pathTuples |> List.iter check

module Main =
    let euroTuples = [
        @"D:\UberSweep\- persisted -\2021-euro", false
        @"D:\UberSweep\- persisted -\2024-euro", false
    ]

    let fifa2018Tuple = @"D:\UberSweep\- persisted -\2018-fifa", true
    let fifa2022Tuple = @"D:\UberSweep\- persisted -\2022-fifa", false

    let fifaTuples = [ fifa2018Tuple; fifa2022Tuple ]

    let rwcTuples = [
        @"D:\UberSweep\- persisted -\2019-rwc", false
        @"D:\UberSweep\- persisted -\2023-rwc", false
    ]

    let allTuples = euroTuples @ fifaTuples @ rwcTuples

    [<EntryPoint>]
    let main _ =
        Log.Logger <- LoggerConfiguration().MinimumLevel.Debug().WriteTo.Console().CreateLogger()

        let logger = Log.Logger

        Checker<UserEvent, User>("users", UserHelper(), logger).Check allTuples
        Checker<NewsEvent, Post>("news", NewsHelper(), logger).Check allTuples

        Checker<SquadEvent<Group6, PlayerTypeFootball>, Squad<Group6, PlayerTypeFootball>>(
            "squads",
            SquadHelper<Group6, PlayerTypeFootball>(),
            logger
        )
            .Check
            euroTuples

        Checker<SquadEvent<Group8, PlayerTypeFootball>, Squad<Group8, PlayerTypeFootball>>(
            "squads",
            SquadHelper<Group8, PlayerTypeFootball>(),
            logger
        )
            .Check
            fifaTuples

        Checker<SquadEvent<Group4, PlayerTypeRugby>, Squad<Group4, PlayerTypeRugby>>(
            "squads",
            SquadHelper<Group4, PlayerTypeRugby>(),
            logger
        )
            .Check
            rwcTuples

        Checker<
            FixtureEvent<StageEuro, UnconfirmedEuro, MatchEventFootball>,
            Fixture<StageEuro, UnconfirmedEuro, MatchEventFootball>
         >(
            "fixtures",
            FixtureHelper<StageEuro, UnconfirmedEuro, MatchEventFootball>(),
            logger
        )
            .Check
            euroTuples

        Checker<
            FixtureEvent<StageFifa, UnconfirmedFifa, MatchEventFootball>,
            Fixture<StageFifa, UnconfirmedFifa, MatchEventFootball>
         >(
            "fixtures",
            FixtureHelper<StageFifa, UnconfirmedFifa, MatchEventFootball>(),
            logger
        )
            .Check
            [ fifa2018Tuple ]

        Checker<
            FixtureEvent<StageFifa, UnconfirmedFifaV2, MatchEventFootball>,
            Fixture<StageFifa, UnconfirmedFifaV2, MatchEventFootball>
         >(
            "fixtures",
            FixtureHelper<StageFifa, UnconfirmedFifaV2, MatchEventFootball>(),
            logger
        )
            .Check
            [ fifa2022Tuple ]

        Checker<
            FixtureEvent<StageRwc, UnconfirmedRwc, MatchEventRugby>,
            Fixture<StageRwc, UnconfirmedRwc, MatchEventRugby>
         >(
            "fixtures",
            FixtureHelper<StageRwc, UnconfirmedRwc, MatchEventRugby>(),
            logger
        )
            .Check
            rwcTuples

        0
