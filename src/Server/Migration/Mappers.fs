namespace Aornota.Ubersweep.Server.Migration

open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Migration.Events
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Shared.Entities

open System.Collections.Generic

type MapUserId = UserId' -> UserId

[<AutoOpen>]
module Mappers =
    let private mapDraftId (DraftId'.DraftId guid) = DraftId.FromGuid guid
    let private mapFixtureId (FixtureId'.FixtureId guid) = FixtureId.FromGuid guid
    let private mapMatchEventId (MatchEventId'.MatchEventId guid) = MatchEventId.FromGuid guid
    let private mapPlayerId (PlayerId'.PlayerId guid) = PlayerId.FromGuid guid
    let private mapSquadId (SquadId'.SquadId guid) = SquadId.FromGuid guid

    let private mapMatchEventsFootabll (matchEvents: Dictionary<MatchEventId', MatchEventFootball'>) =
        let mapMatchEventFootball matchEvent =
            let mapPenaltyOutcome =
                function
                | PenaltyOutcome'.Scored -> PenaltyOutcome.Scored
                | PenaltyOutcome'.Missed -> PenaltyOutcome.Missed
                | PenaltyOutcome'.Saved(squadId, playerId) ->
                    PenaltyOutcome.Saved(mapSquadId squadId, mapPlayerId playerId)

            match matchEvent with
            | MatchEventFootball'.Goal(squadId, playerId, assistedBy) ->
                Goal(mapSquadId squadId, mapPlayerId playerId, assistedBy |> Option.map mapPlayerId)
            | MatchEventFootball'.OwnGoal(squadId, playerId) -> OwnGoal(mapSquadId squadId, mapPlayerId playerId)
            | MatchEventFootball'.Penalty(squadId, playerId, penaltyOutcome) ->
                Penalty(mapSquadId squadId, mapPlayerId playerId, mapPenaltyOutcome penaltyOutcome)
            | MatchEventFootball'.YellowCard(squadId, playerId) ->
                MatchEventFootball.YellowCard(mapSquadId squadId, mapPlayerId playerId)
            | MatchEventFootball'.RedCard(squadId, playerId) ->
                MatchEventFootball.RedCard(mapSquadId squadId, mapPlayerId playerId)
            | MatchEventFootball'.CleanSheet(squadId, playerId) -> CleanSheet(mapSquadId squadId, mapPlayerId playerId)
            | MatchEventFootball'.PenaltyShootout(homeScore, awayScore) -> PenaltyShootout(homeScore, awayScore)
            | MatchEventFootball'.ManOfTheMatch(squadId, playerId) ->
                MatchEventFootball.ManOfTheMatch(mapSquadId squadId, mapPlayerId playerId)

        matchEvents
        |> List.ofSeq
        |> List.map (fun kvp -> mapMatchEventId kvp.Key, mapMatchEventFootball kvp.Value)
        |> Map.ofList

    let private mapPLayersFootball (players: Dictionary<PlayerId', Player<PlayerTypeFootball>>) =
        players
        |> List.ofSeq
        |> List.map (fun kvp -> mapPlayerId kvp.Key, kvp.Value)
        |> Map.ofList

    let private mapParticipant mapUnconfirmed =
        function
        | Participant'.Confirmed squadId -> Confirmed(mapSquadId squadId)
        | Participant'.Unconfirmed unconfirmed -> Unconfirmed(mapUnconfirmed unconfirmed)

    let mapDraft (draft: Draft', mapUserId: MapUserId) =
        let mapDraftPick =
            function
            | DraftPick'.TeamPicked sqaudId -> TeamPicked(mapSquadId sqaudId)
            | DraftPick'.PlayerPicked(squadId, playerId) -> PlayerPicked(mapSquadId squadId, mapPlayerId playerId)

        let mapDraftPicks (draftPicks: (DraftPick' * PickedBy') list) =
            draftPicks
            |> List.map (fun (draftPick, (userId, draftOrdinal, dateTimeOffset)) ->
                mapDraftPick draftPick, (mapUserId userId, draftOrdinal, dateTimeOffset))

        let mapProcessingEvent (event: ProcessingEvent') =
            let mapIgnoredPlayers (ignored: (UserId' * (SquadId' * PlayerId') list) list) =
                ignored
                |> List.map (fun (userId, pairs) ->
                    let mappedPairs =
                        pairs
                        |> List.map (fun (squadId, playerId) -> mapSquadId squadId, mapPlayerId playerId)

                    mapUserId userId, mappedPairs)

            let mapIgnoredDraftPicks (ignored: (UserId' * DraftPick' list) list) =
                ignored
                |> List.map (fun (userId, draftPicks) -> mapUserId userId, draftPicks |> List.map mapDraftPick)

            match event with
            | ProcessingEvent'.ProcessingStarted seed -> ProcessingStarted seed
            | ProcessingEvent'.WithdrawnPlayersIgnored ignored -> WithdrawnPlayersIgnored(mapIgnoredPlayers ignored)
            | ProcessingEvent'.RoundStarted round -> RoundStarted round
            | ProcessingEvent'.AlreadyPickedIgnored ignored -> AlreadyPickedIgnored(mapIgnoredDraftPicks ignored)
            | ProcessingEvent'.NoLongerRequiredIgnored ignored -> NoLongerRequiredIgnored(mapIgnoredDraftPicks ignored)
            | ProcessingEvent'.UncontestedPick(draftPick, userId) ->
                UncontestedPick(mapDraftPick draftPick, mapUserId userId)
            | ProcessingEvent'.ContestedPick(draftPick, userDetails, winner) ->
                let userDetails =
                    userDetails
                    |> List.map (fun (userId, pickPriority, random) -> mapUserId userId, pickPriority, random)

                ContestedPick(mapDraftPick draftPick, userDetails, mapUserId winner)
            | ProcessingEvent'.PickPriorityChanged(userId, pickPriority) ->
                PickPriorityChanged(mapUserId userId, pickPriority)
            | ProcessingEvent'.Picked(draftOrdinal, draftPick, userId, timestamp) ->
                Picked(draftOrdinal, mapDraftPick draftPick, mapUserId userId, timestamp)

        let mapPickPriorities (pickPriorities: Dictionary<UserId', uint32>) =
            pickPriorities
            |> List.ofSeq
            |> List.map (fun kvp -> mapUserId kvp.Key, kvp.Value)
            |> Map.ofList

        let mapDraftStatus =
            function
            | DraftStatus'.ConstrainedDraft(draftOrdinal, ConstrainedStatus'.PendingOpen(starts, ends)) ->
                ConstrainedDraft(draftOrdinal, PendingOpen(starts, ends))
            | DraftStatus'.ConstrainedDraft(draftOrdinal, ConstrainedStatus'.Opened ends) ->
                ConstrainedDraft(draftOrdinal, Opened ends)
            | DraftStatus'.ConstrainedDraft(draftOrdinal, ConstrainedStatus'.PendingProcessing) ->
                ConstrainedDraft(draftOrdinal, PendingProcessing)
            | DraftStatus'.ConstrainedDraft(draftOrdinal, ConstrainedStatus'.Processing _) ->
                ConstrainedDraft(draftOrdinal, Processing)
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processed(draftPicks, processingEvents, pickPriorities)) ->
                ConstrainedDraft(
                    draftOrdinal,
                    Processed(
                        mapDraftPicks draftPicks,
                        processingEvents |> List.map mapProcessingEvent,
                        mapPickPriorities pickPriorities
                    )
                )
            | DraftStatus'.UnconstrainedDraft UnconstrainedStatus'.PendingFreeSelection ->
                UnconstrainedDraft PendingFreeSelection
            | DraftStatus'.UnconstrainedDraft(UnconstrainedStatus'.FreeSelection draftPicks) ->
                UnconstrainedDraft(FreeSelection(mapDraftPicks draftPicks))

        {
            DraftCommon = {
                DraftStatus = mapDraftStatus draft.DraftStatus
            }
        }

    let mapFixtureEuro (fixture: Fixture'<StageEuro, UnconfirmedEuro', MatchEventFootball'>) =
        let mapUnconfirmedEuro =
            function
            | UnconfirmedEuro'.Winner stage -> UnconfirmedEuro.Winner stage
            | UnconfirmedEuro'.RunnerUp group -> UnconfirmedEuro.RunnerUp group
            | UnconfirmedEuro'.ThirdPlace groups -> UnconfirmedEuro.ThirdPlace groups

        let mapParticipantEuro = mapParticipant mapUnconfirmedEuro

        {
            FixtureCommon = {
                Stage = fixture.Stage
                HomeParticipant = mapParticipantEuro fixture.HomeParticipant
                AwayParticipant = mapParticipantEuro fixture.AwayParticipant
                KickOff = fixture.KickOff
                Cancelled = fixture.Cancelled
                MatchEvents = mapMatchEventsFootabll fixture.MatchEvents
            }
        }

    let mapFixtureFifa (fixture: Fixture'<StageFifa, UnconfirmedFifa', MatchEventFootball'>) =
        let mapUnconfirmedFifa =
            function
            | UnconfirmedFifa'.Winner stage -> Winner stage
            | UnconfirmedFifa'.RunnerUp group -> RunnerUp group
            | UnconfirmedFifa'.Loser semiFinalOrdinal -> Loser(StageFifa.SemiFinal semiFinalOrdinal)

        let mapParticipantFifa = mapParticipant mapUnconfirmedFifa

        {
            FixtureCommon = {
                Stage = fixture.Stage
                HomeParticipant = mapParticipantFifa fixture.HomeParticipant
                AwayParticipant = mapParticipantFifa fixture.AwayParticipant
                KickOff = fixture.KickOff
                Cancelled = fixture.Cancelled
                MatchEvents = mapMatchEventsFootabll fixture.MatchEvents
            }
        }

    let mapFixtureFifaV2 (fixture: Fixture'<StageFifa, UnconfirmedFifaV2', MatchEventFootball'>) =
        let mapUnconfirmedFifaV2 =
            function
            | UnconfirmedFifaV2'.Winner stage -> Winner stage
            | UnconfirmedFifaV2'.RunnerUp group -> RunnerUp group
            | UnconfirmedFifaV2'.Loser stage -> Loser stage

        let mapParticipantFifaV2 = mapParticipant mapUnconfirmedFifaV2

        {
            FixtureCommon = {
                Stage = fixture.Stage
                HomeParticipant = mapParticipantFifaV2 fixture.HomeParticipant
                AwayParticipant = mapParticipantFifaV2 fixture.AwayParticipant
                KickOff = fixture.KickOff
                Cancelled = fixture.Cancelled
                MatchEvents = mapMatchEventsFootabll fixture.MatchEvents
            }
        }

    let mapFixtureRwc (fixture: Fixture'<StageRwc, UnconfirmedRwc', MatchEventRugby'>) =
        let mapMatchEventsRugby (matchEvents: Dictionary<MatchEventId', MatchEventRugby'>) =
            let mapMatchEventRugby matchEvent =
                match matchEvent with
                | MatchEventRugby'.Try(squadId, playerId) -> Try(mapSquadId squadId, mapPlayerId playerId)
                | MatchEventRugby'.PenaltyTry squadId -> PenaltyTry(mapSquadId squadId)
                | MatchEventRugby'.PenaltyKick(squadId, playerId, kickOutcome) ->
                    PenaltyKick(mapSquadId squadId, mapPlayerId playerId, kickOutcome)
                | MatchEventRugby'.Conversion(squadId, playerId, kickOutcome) ->
                    Conversion(mapSquadId squadId, mapPlayerId playerId, kickOutcome)
                | MatchEventRugby'.DropGoal(squadId, playerId) -> DropGoal(mapSquadId squadId, mapPlayerId playerId)
                | MatchEventRugby'.YellowCard(squadId, playerId) -> YellowCard(mapSquadId squadId, mapPlayerId playerId)
                | MatchEventRugby'.RedCard(squadId, playerId) -> RedCard(mapSquadId squadId, mapPlayerId playerId)
                | MatchEventRugby'.ManOfTheMatch(squadId, playerId) ->
                    ManOfTheMatch(mapSquadId squadId, mapPlayerId playerId)

            matchEvents
            |> List.ofSeq
            |> List.map (fun kvp -> mapMatchEventId kvp.Key, mapMatchEventRugby kvp.Value)
            |> Map.ofList

        let mapUnconfirmedRwc =
            function
            | GroupRunnerUp group -> RunnerUp group
            | StageWinner stage -> Winner stage
            | SemiFinalLoser semiFinalOrdinal -> Loser(SemiFinal semiFinalOrdinal)

        let mapParticipantRwc = mapParticipant mapUnconfirmedRwc

        {
            FixtureCommon = {
                Stage = fixture.Stage
                HomeParticipant = mapParticipantRwc fixture.HomeParticipant
                AwayParticipant = mapParticipantRwc fixture.AwayParticipant
                KickOff = fixture.KickOff
                Cancelled = fixture.Cancelled
                MatchEvents = mapMatchEventsRugby fixture.MatchEvents
            }
        }

    let mapPost (post: Post', mapUserId: MapUserId) =
        let mapPostType =
            function
            | PostType'.Standard -> Standard
            | PostType'.MatchResult fixtureId -> MatchResult(mapFixtureId fixtureId)

        {
            PostCommon = {
                PostType = mapPostType post.PostType
                MessageText = post.MessageText
                Timestamp = post.Timestamp
                Removed = post.Removed
            }
            UserId = mapUserId post.UserId
        }

    let mapSquadEuro (squad: Squad'<GroupAToF, PlayerTypeFootball>) = {
        SquadCommon = {
            SquadName = squad.SquadName
            Group = squad.Group
            Seeding = squad.Seeding
            CoachName = squad.CoachName
            Eliminated = squad.Eliminated
            Players = mapPLayersFootball squad.Players
        }
    }

    let mapSquadFifa (squad: Squad'<GroupAToH, PlayerTypeFootball>) = {
        SquadCommon = {
            SquadName = squad.SquadName
            Group = squad.Group
            Seeding = squad.Seeding
            CoachName = squad.CoachName
            Eliminated = squad.Eliminated
            Players = mapPLayersFootball squad.Players
        }
    }

    let mapSquadRwc (squad: Squad'<GroupAToD, PlayerTypeRugby>) =
        let mapPLayersRugby (players: Dictionary<PlayerId', Player<PlayerTypeRugby>>) =
            players
            |> List.ofSeq
            |> List.map (fun kvp -> mapPlayerId kvp.Key, kvp.Value)
            |> Map.ofList

        {
            SquadCommon = {
                SquadName = squad.SquadName
                Group = squad.Group
                Seeding = squad.Seeding
                CoachName = squad.CoachName
                Eliminated = squad.Eliminated
                Players = mapPLayersRugby squad.Players
            }
        }

    let mapUser (user: User') = {
        UserCommon = {
            UserName = user.UserName
            UserType = user.UserType
            MustChangePasswordReason = user.MustChangePasswordReason
        }
        PasswordSalt = user.PasswordSalt
        PasswordHash = user.PasswordHash
    }

    let mapUserDraft (userDraft: UserDraft', mapUserId: MapUserId) =
        let mapUserDraftPicks (userDraftPicks: Dictionary<UserDraftPick', int>) =
            let mapUserDraftPick =
                function
                | UserDraftPick'.TeamPick sqaudId -> TeamPick(mapSquadId sqaudId)
                | UserDraftPick'.PlayerPick(squadId, playerId) -> PlayerPick(mapSquadId squadId, mapPlayerId playerId)

            userDraftPicks
            |> List.ofSeq
            |> List.map (fun kvp -> mapUserDraftPick kvp.Key, kvp.Value)
            |> Map.ofList

        let userId, draftId = userDraft.UserDraftKey

        {
            UserDraftCommon = {
                UserDraftKey = mapUserId userId, mapDraftId draftId
                UserDraftPicks = mapUserDraftPicks userDraft.UserDraftPicks
            }
        }
