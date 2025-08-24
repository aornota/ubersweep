namespace Aornota.Ubersweep.Server.Migration

open Aornota.Ubersweep.Migration
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open System.Collections.Generic

type MapUserId = Domain.UserId -> UserId

[<AutoOpen>]
module Mappers =
    let private mapDraftId (Domain.DraftId guid) = DraftId.FromGuid guid
    let private mapFixtureId (Domain.FixtureId guid) = FixtureId.FromGuid guid
    let private mapMatchEventId (Domain.MatchEventId guid) = MatchEventId.FromGuid guid
    let private mapPlayerId (Domain.PlayerId guid) = PlayerId.FromGuid guid
    let private mapSquadId (Domain.SquadId guid) = SquadId.FromGuid guid

    let private mapMatchEventsFootabll (matchEvents: Dictionary<Domain.MatchEventId, Domain.MatchEventFootball>) =
        let mapMatchEventFootball matchEvent =
            let mapPenaltyOutcome =
                function
                | Domain.Scored -> Scored
                | Domain.Missed -> PenaltyOutcome.Missed
                | Domain.Saved(squadId, playerId) -> Saved(mapSquadId squadId, mapPlayerId playerId)

            match matchEvent with
            | Domain.Goal(squadId, playerId, assistedBy) ->
                Goal(mapSquadId squadId, mapPlayerId playerId, assistedBy |> Option.map mapPlayerId)
            | Domain.OwnGoal(squadId, playerId) -> OwnGoal(mapSquadId squadId, mapPlayerId playerId)
            | Domain.Penalty(squadId, playerId, penaltyOutcome) ->
                Penalty(mapSquadId squadId, mapPlayerId playerId, mapPenaltyOutcome penaltyOutcome)
            | Domain.YellowCard(squadId, playerId) ->
                MatchEventFootball.YellowCard(mapSquadId squadId, mapPlayerId playerId)
            | Domain.RedCard(squadId, playerId) -> MatchEventFootball.RedCard(mapSquadId squadId, mapPlayerId playerId)
            | Domain.CleanSheet(squadId, playerId) -> CleanSheet(mapSquadId squadId, mapPlayerId playerId)
            | Domain.PenaltyShootout(homeScore, awayScore) -> PenaltyShootout(homeScore, awayScore)
            | Domain.ManOfTheMatch(squadId, playerId) ->
                MatchEventFootball.ManOfTheMatch(mapSquadId squadId, mapPlayerId playerId)

        matchEvents
        |> List.ofSeq
        |> List.map (fun kvp -> mapMatchEventId kvp.Key, mapMatchEventFootball kvp.Value)
        |> Map.ofList

    let private mapPLayersFootball (players: Dictionary<Domain.PlayerId, Player<PlayerTypeFootball>>) =
        players
        |> List.ofSeq
        |> List.map (fun kvp -> mapPlayerId kvp.Key, kvp.Value)
        |> Map.ofList

    let private mapParticipant mapUnconfirmed =
        function
        | Domain.Confirmed squadId -> Confirmed(mapSquadId squadId)
        | Domain.Unconfirmed unconfirmed -> Unconfirmed(mapUnconfirmed unconfirmed)

    let mapDraft (draft: Events.Draft, mapUserId: MapUserId) =
        let mapDraftPick =
            function
            | Domain.TeamPicked sqaudId -> TeamPicked(mapSquadId sqaudId)
            | Domain.PlayerPicked(squadId, playerId) -> PlayerPicked(mapSquadId squadId, mapPlayerId playerId)

        let mapDraftPicks (draftPicks: (Domain.DraftPick * Domain.PickedBy) list) =
            draftPicks
            |> List.map (fun (draftPick, (userId, draftOrdinal, dateTimeOffset)) ->
                mapDraftPick draftPick, (mapUserId userId, draftOrdinal, dateTimeOffset))

        let mapProcessingEvent (event: Domain.ProcessingEvent) =
            let mapIgnoredPlayers (ignored: (Domain.UserId * (Domain.SquadId * Domain.PlayerId) list) list) =
                ignored
                |> List.map (fun (userId, pairs) ->
                    let mappedPairs =
                        pairs
                        |> List.map (fun (squadId, playerId) -> mapSquadId squadId, mapPlayerId playerId)

                    mapUserId userId, mappedPairs)

            let mapIgnoredDraftPicks (ignored: (Domain.UserId * Domain.DraftPick list) list) =
                ignored
                |> List.map (fun (userId, draftPicks) -> mapUserId userId, draftPicks |> List.map mapDraftPick)

            match event with
            | Domain.ProcessingStarted seed -> ProcessingStarted seed
            | Domain.WithdrawnPlayersIgnored ignored -> WithdrawnPlayersIgnored(mapIgnoredPlayers ignored)
            | Domain.RoundStarted round -> RoundStarted round
            | Domain.AlreadyPickedIgnored ignored -> AlreadyPickedIgnored(mapIgnoredDraftPicks ignored)
            | Domain.NoLongerRequiredIgnored ignored -> NoLongerRequiredIgnored(mapIgnoredDraftPicks ignored)
            | Domain.UncontestedPick(draftPick, userId) -> UncontestedPick(mapDraftPick draftPick, mapUserId userId)
            | Domain.ContestedPick(draftPick, userDetails, winner) ->
                let userDetails =
                    userDetails
                    |> List.map (fun (userId, pickPriority, random) -> mapUserId userId, pickPriority, random)

                ContestedPick(mapDraftPick draftPick, userDetails, mapUserId winner)
            | Domain.PickPriorityChanged(userId, pickPriority) -> PickPriorityChanged(mapUserId userId, pickPriority)
            | Domain.Picked(draftOrdinal, draftPick, userId, timestamp) ->
                Picked(draftOrdinal, mapDraftPick draftPick, mapUserId userId, timestamp)

        let mapPickPriorities (pickPriorities: Dictionary<Domain.UserId, uint32>) =
            pickPriorities
            |> List.ofSeq
            |> List.map (fun kvp -> mapUserId kvp.Key, kvp.Value)
            |> Map.ofList

        let mapDraftStatus =
            function
            | Domain.ConstrainedDraft(draftOrdinal, Domain.PendingOpen(starts, ends)) ->
                ConstrainedDraft(draftOrdinal, PendingOpen(starts, ends))
            | Domain.ConstrainedDraft(draftOrdinal, Domain.Opened ends) -> ConstrainedDraft(draftOrdinal, Opened ends)
            | Domain.ConstrainedDraft(draftOrdinal, Domain.PendingProcessing) ->
                ConstrainedDraft(draftOrdinal, PendingProcessing)
            | Domain.ConstrainedDraft(draftOrdinal, Domain.Processing _) -> ConstrainedDraft(draftOrdinal, Processing)
            | Domain.ConstrainedDraft(draftOrdinal, Domain.Processed(draftPicks, processingEvents, pickPriorities)) ->
                ConstrainedDraft(
                    draftOrdinal,
                    Processed(
                        mapDraftPicks draftPicks,
                        processingEvents |> List.map mapProcessingEvent,
                        mapPickPriorities pickPriorities
                    )
                )
            | Domain.UnconstrainedDraft Domain.PendingFreeSelection -> UnconstrainedDraft PendingFreeSelection
            | Domain.UnconstrainedDraft(Domain.FreeSelection draftPicks) ->
                UnconstrainedDraft(FreeSelection(mapDraftPicks draftPicks))

        {
            DraftCommon = {
                DraftStatus = mapDraftStatus draft.DraftStatus
            }
        }

    let mapFixtureEuro (fixture: Events.Fixture<StageEuro, Domain.UnconfirmedEuro, Domain.MatchEventFootball>) =
        let mapUnconfirmedEuro =
            function
            | Domain.UnconfirmedEuro.Winner stage -> UnconfirmedEuro.Winner stage
            | Domain.UnconfirmedEuro.RunnerUp group -> UnconfirmedEuro.RunnerUp group
            | Domain.UnconfirmedEuro.ThirdPlace groups -> UnconfirmedEuro.ThirdPlace groups

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

    let mapFixtureFifa (fixture: Events.Fixture<StageFifa, Domain.UnconfirmedFifa, Domain.MatchEventFootball>) =
        let mapUnconfirmedFifa =
            function
            | Domain.UnconfirmedFifa.Winner stage -> Winner stage
            | Domain.UnconfirmedFifa.RunnerUp group -> RunnerUp group
            | Domain.UnconfirmedFifa.Loser semiFinalOrdinal -> Loser(StageFifa.SemiFinal semiFinalOrdinal)

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

    let mapFixtureFifaV2 (fixture: Events.Fixture<StageFifa, Domain.UnconfirmedFifaV2, Domain.MatchEventFootball>) =
        let mapUnconfirmedFifaV2 =
            function
            | Domain.UnconfirmedFifaV2.Winner stage -> Winner stage
            | Domain.UnconfirmedFifaV2.RunnerUp group -> RunnerUp group
            | Domain.UnconfirmedFifaV2.Loser stage -> Loser stage

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

    let mapFixtureRwc (fixture: Events.Fixture<StageRwc, Domain.UnconfirmedRwc, Domain.MatchEventRugby>) =
        let mapMatchEventsRugby (matchEvents: Dictionary<Domain.MatchEventId, Domain.MatchEventRugby>) =
            let mapMatchEventRugby matchEvent =
                match matchEvent with
                | Domain.Try(squadId, playerId) -> Try(mapSquadId squadId, mapPlayerId playerId)
                | Domain.PenaltyTry squadId -> PenaltyTry(mapSquadId squadId)
                | Domain.PenaltyKick(squadId, playerId, kickOutcome) ->
                    PenaltyKick(mapSquadId squadId, mapPlayerId playerId, kickOutcome)
                | Domain.Conversion(squadId, playerId, kickOutcome) ->
                    Conversion(mapSquadId squadId, mapPlayerId playerId, kickOutcome)
                | Domain.DropGoal(squadId, playerId) -> DropGoal(mapSquadId squadId, mapPlayerId playerId)
                | Domain.MatchEventRugby.YellowCard(squadId, playerId) ->
                    YellowCard(mapSquadId squadId, mapPlayerId playerId)
                | Domain.MatchEventRugby.RedCard(squadId, playerId) -> RedCard(mapSquadId squadId, mapPlayerId playerId)
                | Domain.MatchEventRugby.ManOfTheMatch(squadId, playerId) ->
                    ManOfTheMatch(mapSquadId squadId, mapPlayerId playerId)

            matchEvents
            |> List.ofSeq
            |> List.map (fun kvp -> mapMatchEventId kvp.Key, mapMatchEventRugby kvp.Value)
            |> Map.ofList

        let mapUnconfirmedRwc =
            function
            | Domain.UnconfirmedRwc.GroupRunnerUp group -> RunnerUp group
            | Domain.UnconfirmedRwc.StageWinner stage -> Winner stage
            | Domain.UnconfirmedRwc.SemiFinalLoser semiFinalOrdinal -> Loser(SemiFinal semiFinalOrdinal)

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

    let mapPost (post: Events.Post, mapUserId: MapUserId) =
        let mapPostType =
            function
            | Domain.Standard -> Standard
            | Domain.MatchResult fixtureId -> MatchResult(mapFixtureId fixtureId)

        {
            PostCommon = {
                PostType = mapPostType post.PostType
                MessageText = post.MessageText
                Timestamp = post.Timestamp
                Removed = post.Removed
            }
            UserId = mapUserId post.UserId
        }

    let mapSquadEuro (squad: Events.Squad<GroupAToF, PlayerTypeFootball>) = {
        SquadCommon = {
            SquadName = squad.SquadName
            Group = squad.Group
            Seeding = squad.Seeding
            CoachName = squad.CoachName
            Eliminated = squad.Eliminated
            Players = mapPLayersFootball squad.Players
        }
    }

    let mapSquadFifa (squad: Events.Squad<GroupAToH, PlayerTypeFootball>) = {
        SquadCommon = {
            SquadName = squad.SquadName
            Group = squad.Group
            Seeding = squad.Seeding
            CoachName = squad.CoachName
            Eliminated = squad.Eliminated
            Players = mapPLayersFootball squad.Players
        }
    }

    let mapSquadRwc (squad: Events.Squad<GroupAToD, PlayerTypeRugby>) =
        let mapPLayersRugby (players: Dictionary<Domain.PlayerId, Player<PlayerTypeRugby>>) =
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

    let mapUser (user: Events.User) = {
        UserCommon = {
            UserName = user.UserName
            UserType = user.UserType
            MustChangePasswordReason = user.MustChangePasswordReason
        }
        PasswordSalt = user.PasswordSalt
        PasswordHash = user.PasswordHash
    }

    let mapUserDraft (userDraft: Events.UserDraft, mapUserId: MapUserId) =
        let mapUserDraftPicks (userDraftPicks: Dictionary<Domain.UserDraftPick, int>) =
            let mapUserDraftPick =
                function
                | Domain.TeamPick sqaudId -> TeamPick(mapSquadId sqaudId)
                | Domain.PlayerPick(squadId, playerId) -> PlayerPick(mapSquadId squadId, mapPlayerId playerId)

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
