namespace Aornota.Ubersweep.Migration

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Migration.Events
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System.IO

type Partition<'group, 'stage, 'unconfirmed, 'playerType, 'matchEvent>(root, useLegacyDeserializer: bool, logger) =
    [<Literal>]
    let draftsSubPath = "drafts"

    [<Literal>]
    let fixturesSubPath = "fixtures"

    [<Literal>]
    let postsSubPath = "news"

    [<Literal>]
    let squadsSubPath = "squads"

    [<Literal>]
    let usersSubPath = "users"

    [<Literal>]
    let userDraftsSubPath = "user-drafts"

    let logger = SourcedLogger.Create<Partition<_, _, _, _, _>>(root, logger)

    do logger.Information("Using legacy deserializer: {useLegacyDeserializer}", useLegacyDeserializer)

    member private _.Read<'event, 'entity>(subPath: string, helper: IHelper<'event, 'entity>) = result {
        let path = Path.Combine(root, subPath)

        let! list = Reader<'event>($"{path}", useLegacyDeserializer, logger).ReadAll()

        return!
            list
            |> List.map (fun (guid, events) ->
                match helper.ApplyEvents(events |> List.map _.Event) with
                | Ok(entity, rvn) -> Ok(guid, events, entity, rvn)
                | Error error -> Error $"Error applying events for {guid}: {error}")
            |> List.sequenceResultA
    }

    member this.ReadDrafts() =
        logger.Debug("Reading {Draft}s...", nameof Draft')

        let result = this.Read(draftsSubPath, DraftHelper'())

        match result with
        | Ok list -> logger.Debug("...{length} {Draft}/s read", list.Length, nameof Draft')
        | Error errors ->
            logger.Error("...{length} error/s reading {Draft}s: {errors}", errors.Length, nameof Draft', errors)

        result

    member this.ReadFixtures() =
        logger.Debug("Reading {Fixture}s...", nameof Fixture')

        let result =
            this.Read(fixturesSubPath, FixtureHelper'<'stage, 'unconfirmed, 'matchEvent>())

        match result with
        | Ok list -> logger.Debug("...{length} {Fixture}/s read", list.Length, nameof Fixture')
        | Error errors ->
            logger.Error("...{length} error/s reading {Fixture}s: {errors}", errors.Length, nameof Fixture', errors)

        result

    member this.ReadPosts() =
        logger.Debug("Reading {Post}s...", nameof Post')

        let result = this.Read(postsSubPath, NewsHelper'())

        match result with
        | Ok list -> logger.Debug("...{length} {Post}/s read", list.Length, nameof Post')
        | Error errors ->
            logger.Error("...{length} error/s reading {Post}s: {errors}", errors.Length, nameof Post', errors)

        result

    member this.ReadSquads() =
        logger.Debug("Reading {Squad}s...", nameof Squad')

        let result = this.Read(squadsSubPath, SquadHelper'<'group, 'playerType>())

        match result with
        | Ok list -> logger.Debug("...{length} {Squad}/s read", list.Length, nameof Squad')
        | Error errors ->
            logger.Error("...{length} error/s reading {Squad}s: {errors}", errors.Length, nameof Squad', errors)

        result

    member this.ReadUsers() =
        logger.Debug("Reading {User}s...", nameof User')

        let result = this.Read(usersSubPath, UserHelper'())

        match result with
        | Ok list -> logger.Debug("...{length} {User}/s read", list.Length, nameof User')
        | Error errors ->
            logger.Error("...{length} error/s reading {User}s: {errors}", errors.Length, nameof User', errors)

        result

    member this.ReadUserDrafts() =
        logger.Debug("Reading {UserDraft}s...", nameof UserDraft')

        let result = this.Read(userDraftsSubPath, UserDraftHelper'())

        match result with
        | Ok list -> logger.Debug("...{length} {UserDraft}/s read", list.Length, nameof UserDraft')
        | Error errors ->
            logger.Error("...{length} error/s reading {UserDraft}s: {errors}", errors.Length, nameof UserDraft', errors)

        result

[<RequireQualifiedAccess>]
module Partition =
    let fifa2018 (root, logger) =
        Partition<GroupAToH, StageFifa, UnconfirmedFifa', PlayerTypeFootball, MatchEventFootball'>(root, true, logger)

    let rwc2019 (root, logger) =
        Partition<GroupAToD, StageRwc, UnconfirmedRwc', PlayerTypeRugby, MatchEventRugby'>(root, false, logger)

    let euro2020 (root, logger) =
        Partition<GroupAToF, StageEuro, UnconfirmedEuro', PlayerTypeFootball, MatchEventFootball'>(root, false, logger)

    let fifa2022 (root, logger) =
        Partition<GroupAToH, StageFifa, UnconfirmedFifaV2', PlayerTypeFootball, MatchEventFootball'>(
            root,
            false,
            logger
        )

    let rwc2023 (root, logger) =
        Partition<GroupAToD, StageRwc, UnconfirmedRwc', PlayerTypeRugby, MatchEventRugby'>(root, false, logger)

    let euro2024 (root, logger) =
        Partition<GroupAToF, StageEuro, UnconfirmedEuro', PlayerTypeFootball, MatchEventFootball'>(root, false, logger)
