namespace Aornota.Ubersweep.Migration

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Migration.Events
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System.IO
open Thoth.Json.Net

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

    member private _.ReadAsync<'event, 'entity>
        (subPath: string, eventDecoder: Decoder<'event>, helper: IHelper<'event, 'entity>)
        =
        asyncResult {
            let path = Path.Combine(root, subPath)

            let! list = Reader<'event>(path, eventDecoder, useLegacyDeserializer, logger).ReadAllAsync()

            return!
                list
                |> List.map (fun (guid, events) ->
                    match helper.ApplyEvents(events |> List.map _.Event) with
                    | Ok(entity, rvn) -> Ok(guid, events, entity, rvn)
                    | Error error -> Error $"Error applying events for {guid}: {error}")
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when reading all for {path}: {errors}")
        }

    member this.ReadDraftsAsync() = async {
        logger.Debug("Reading {Draft}s...", nameof Draft')

        let draftEventDecoder =
            Decode.Auto.generateDecoderCached<DraftEvent'> (Json.caseStrategy, Json.extraCoders)

        let! result = this.ReadAsync(draftsSubPath, draftEventDecoder, DraftHelper'())

        match result with
        | Ok list -> logger.Debug("...{length} {Draft}/s read", list.Length, nameof Draft')
        | Error error -> logger.Error("...error when reading {Draft}s: {error}", nameof Draft', error)

        return result
    }

    member this.ReadFixturesAsync() = async {
        logger.Debug("Reading {Fixture}s...", nameof Fixture')

        let fixtureEventDecoder =
            Decode.Auto.generateDecoderCached<FixtureEvent'<'stage, 'unconfirmed, 'matchEvent>> (
                Json.caseStrategy,
                Json.extraCoders
            )

        let! result =
            this.ReadAsync(fixturesSubPath, fixtureEventDecoder, FixtureHelper'<'stage, 'unconfirmed, 'matchEvent>())

        match result with
        | Ok list -> logger.Debug("...{length} {Fixture}/s read", list.Length, nameof Fixture')
        | Error error -> logger.Error("...error when reading {Fixture}s: {error}", nameof Fixture', error)

        return result
    }

    member this.ReadPostsAsync() = async {
        logger.Debug("Reading {Post}s...", nameof Post')

        let newsEventDecoder =
            Decode.Auto.generateDecoderCached<NewsEvent'> (Json.caseStrategy, Json.extraCoders)

        let! result = this.ReadAsync(postsSubPath, newsEventDecoder, NewsHelper'())

        match result with
        | Ok list -> logger.Debug("...{length} {Post}/s read", list.Length, nameof Post')
        | Error error -> logger.Error("...error when reading {Post}s: {error}", nameof Post', error)

        return result
    }

    member this.ReadSquadsAsync() = async {
        logger.Debug("Reading {Squad}s...", nameof Squad')

        let squadEventDecoder =
            Decode.Auto.generateDecoderCached<SquadEvent'<'group, 'playerType>> (Json.caseStrategy, Json.extraCoders)

        let! result = this.ReadAsync(squadsSubPath, squadEventDecoder, SquadHelper'<'group, 'playerType>())

        match result with
        | Ok list -> logger.Debug("...{length} {Squad}/s read", list.Length, nameof Squad')
        | Error error -> logger.Error("...error when reading {Squad}s: {error}", nameof Squad', error)

        return result
    }

    member this.ReadUsersAsync() = async {
        logger.Debug("Reading {User}s...", nameof User')

        let userEventDecoder =
            Decode.Auto.generateDecoderCached<UserEvent'> (Json.caseStrategy, Json.extraCoders)

        let! result = this.ReadAsync(usersSubPath, userEventDecoder, UserHelper'())

        match result with
        | Ok list -> logger.Debug("...{length} {User}/s read", list.Length, nameof User')
        | Error error -> logger.Error("...error when reading {User}s: {error}", nameof User', error)

        return result
    }

    member this.ReadUserDraftsAsync() = async {
        logger.Debug("Reading {UserDraft}s...", nameof UserDraft')

        let userDraftEventDecoder =
            Decode.Auto.generateDecoderCached<UserDraftEvent'> (Json.caseStrategy, Json.extraCoders)

        let! result = this.ReadAsync(userDraftsSubPath, userDraftEventDecoder, UserDraftHelper'())

        match result with
        | Ok list -> logger.Debug("...{length} {UserDraft}/s read", list.Length, nameof UserDraft')
        | Error error -> logger.Error("...error when reading {UserDraft}s: {error}", nameof UserDraft', error)

        return result
    }

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
