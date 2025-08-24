namespace Aornota.Ubersweep.Server.Migration

open Aornota.Ubersweep.Migration
open Aornota.Ubersweep.Server.Common
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

    let private mapMatchEventFootball matchEvent =
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

    let private mapPLayersFootball (players: Dictionary<Domain.PlayerId, Player<PlayerTypeFootball>>) =
        players
        |> List.ofSeq
        |> List.map (fun kvp -> mapPlayerId kvp.Key, kvp.Value)
        |> Map.ofList

    let private mapParticipant mapUnconfirmed =
        function
        | Domain.Confirmed squadId -> Confirmed(mapSquadId squadId)
        | Domain.Unconfirmed unconfirmed -> Unconfirmed(mapUnconfirmed unconfirmed)

    // TODO: Remove mapDraftEvents | mapFixtureEvents[Euro|Fifa[V2]|Rwc] | mapUserDraftEvents...

    let mapDraftEvents (events: Event<Events.DraftEvent> list, mapUserId: MapUserId) =
        let mapDraftPick =
            function
            | Domain.TeamPicked sqaudId -> TeamPicked(mapSquadId sqaudId)
            | Domain.PlayerPicked(squadId, playerId) -> PlayerPicked(mapSquadId squadId, mapPlayerId playerId)

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

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.DraftCreated(_, DraftOrdinal draftOrdinal, draftType) ->
                    DraftCreated(DraftOrdinal draftOrdinal, draftType) :> IEvent
                | Events.DraftOpened _ -> DraftOpened
                | Events.DraftPendingProcessing _ -> DraftPendingProcessing
                | Events.ProcessingStarted(_, seed) -> DraftEvent.ProcessingStarted seed
                | Events.WithdrawnPlayersIgnored(_, ignored) ->
                    DraftEvent.WithdrawnPlayersIgnored(mapIgnoredPlayers ignored)
                | Events.RoundStarted(_, round) -> DraftEvent.RoundStarted round
                | Events.AlreadyPickedIgnored(_, ignored) ->
                    DraftEvent.AlreadyPickedIgnored(mapIgnoredDraftPicks ignored)
                | Events.NoLongerRequiredIgnored(_, ignored) ->
                    DraftEvent.NoLongerRequiredIgnored(mapIgnoredDraftPicks ignored)
                | Events.UncontestedPick(_, draftPick, userId) ->
                    DraftEvent.UncontestedPick(mapDraftPick draftPick, mapUserId userId)
                | Events.ContestedPick(_, draftPick, userDetails, winner) ->
                    let mappedUserDetails =
                        userDetails
                        |> List.map (fun (userId, pickPriority, random) -> mapUserId userId, pickPriority, random)

                    DraftEvent.ContestedPick(mapDraftPick draftPick, mappedUserDetails, mapUserId winner)
                | Events.PickPriorityChanged(_, userId, pickPriority) ->
                    DraftEvent.PickPriorityChanged(mapUserId userId, pickPriority)
                | Events.Picked(_, DraftOrdinal draftOrdinal, draftPick, userId, timestamp) ->
                    DraftEvent.Picked(DraftOrdinal draftOrdinal, mapDraftPick draftPick, mapUserId userId, timestamp)
                | Events.DraftProcessed _ -> DraftProcessed
                | Events.DraftFreeSelection _ -> DraftFreeSelection
                | Events.FreePick(_, draftPick, userId, timestamp) ->
                    FreePick(mapDraftPick draftPick, mapUserId userId, timestamp)

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapFixtureEventsEuro
        (
            events: Event<Events.FixtureEvent<StageEuro, Domain.UnconfirmedEuro, Domain.MatchEventFootball>> list,
            mapUserId: MapUserId
        ) =
        let mapUnconfirmedEuro =
            function
            | Domain.UnconfirmedEuro.Winner stage -> UnconfirmedEuro.Winner stage
            | Domain.UnconfirmedEuro.RunnerUp group -> UnconfirmedEuro.RunnerUp group
            | Domain.UnconfirmedEuro.ThirdPlace groups -> UnconfirmedEuro.ThirdPlace groups

        let mapParticipantEuro = mapParticipant mapUnconfirmedEuro

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.FixtureCreated(_, stage, homeParticipant, awayParticipant, kickOff) ->
                    FixtureCreated(
                        stage,
                        mapParticipantEuro homeParticipant,
                        mapParticipantEuro awayParticipant,
                        kickOff
                    )
                    :> IEvent
                | Events.ParticipantConfirmed(_, role, squadId) -> ParticipantConfirmed(role, mapSquadId squadId)
                | Events.MatchEventAdded(_, matchEventId, matchEvent) ->
                    MatchEventAdded(mapMatchEventId matchEventId, mapMatchEventFootball matchEvent)
                | Events.MatchEventRemoved(_, matchEventId) -> MatchEventRemoved(mapMatchEventId matchEventId)
                | Events.FixtureCancelled _ -> FixtureCancelled

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapFixtureEventsFifa
        (
            events: Event<Events.FixtureEvent<StageFifa, Domain.UnconfirmedFifa, Domain.MatchEventFootball>> list,
            mapUserId: MapUserId
        ) =
        let mapUnconfirmedFifa =
            function
            | Domain.UnconfirmedFifa.Winner stage -> Winner stage
            | Domain.UnconfirmedFifa.RunnerUp group -> RunnerUp group
            | Domain.UnconfirmedFifa.Loser semiFinalOrdinal -> Loser(StageFifa.SemiFinal semiFinalOrdinal)

        let mapParticipantFifa = mapParticipant mapUnconfirmedFifa

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.FixtureCreated(_, stage, homeParticipant, awayParticipant, kickOff) ->
                    FixtureCreated(
                        stage,
                        mapParticipantFifa homeParticipant,
                        mapParticipantFifa awayParticipant,
                        kickOff
                    )
                    :> IEvent
                | Events.ParticipantConfirmed(_, role, squadId) -> ParticipantConfirmed(role, mapSquadId squadId)
                | Events.MatchEventAdded(_, matchEventId, matchEvent) ->
                    MatchEventAdded(mapMatchEventId matchEventId, mapMatchEventFootball matchEvent)
                | Events.MatchEventRemoved(_, matchEventId) -> MatchEventRemoved(mapMatchEventId matchEventId)
                | Events.FixtureCancelled _ -> FixtureCancelled

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapFixtureEventsFifaV2
        (
            events: Event<Events.FixtureEvent<StageFifa, Domain.UnconfirmedFifaV2, Domain.MatchEventFootball>> list,
            mapUserId: MapUserId
        ) =
        let mapUnconfirmedFifaV2 =
            function
            | Domain.UnconfirmedFifaV2.Winner stage -> Winner stage
            | Domain.UnconfirmedFifaV2.RunnerUp group -> RunnerUp group
            | Domain.UnconfirmedFifaV2.Loser stage -> Loser stage

        let mapParticipantFifa = mapParticipant mapUnconfirmedFifaV2

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.FixtureCreated(_, stage, homeParticipant, awayParticipant, kickOff) ->
                    FixtureCreated(
                        stage,
                        mapParticipantFifa homeParticipant,
                        mapParticipantFifa awayParticipant,
                        kickOff
                    )
                    :> IEvent
                | Events.ParticipantConfirmed(_, role, squadId) -> ParticipantConfirmed(role, mapSquadId squadId)
                | Events.MatchEventAdded(_, matchEventId, matchEvent) ->
                    MatchEventAdded(mapMatchEventId matchEventId, mapMatchEventFootball matchEvent)
                | Events.MatchEventRemoved(_, matchEventId) -> MatchEventRemoved(mapMatchEventId matchEventId)
                | Events.FixtureCancelled _ -> FixtureCancelled

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapFixtureEventsRwc
        (
            events: Event<Events.FixtureEvent<StageRwc, Domain.UnconfirmedRwc, Domain.MatchEventRugby>> list,
            mapUserId: MapUserId
        ) =
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

        let mapUnconfirmedRwc =
            function
            | Domain.UnconfirmedRwc.GroupRunnerUp group -> RunnerUp group
            | Domain.UnconfirmedRwc.StageWinner stage -> Winner stage
            | Domain.UnconfirmedRwc.SemiFinalLoser semiFinalOrdinal -> Loser(SemiFinal semiFinalOrdinal)

        let mapParticipantRwc = mapParticipant mapUnconfirmedRwc

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.FixtureCreated(_, stage, homeParticipant, awayParticipant, kickOff) ->
                    FixtureCreated(
                        stage,
                        mapParticipantRwc homeParticipant,
                        mapParticipantRwc awayParticipant,
                        kickOff
                    )
                    :> IEvent
                | Events.ParticipantConfirmed(_, role, squadId) -> ParticipantConfirmed(role, mapSquadId squadId)
                | Events.MatchEventAdded(_, matchEventId, matchEvent) ->
                    MatchEventAdded(mapMatchEventId matchEventId, mapMatchEventRugby matchEvent)
                | Events.MatchEventRemoved(_, matchEventId) -> MatchEventRemoved(mapMatchEventId matchEventId)
                | Events.FixtureCancelled _ -> FixtureCancelled

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    // TODO: mapDraft (draft: Events.Draft, mapUserId: MapUserId) =...

    // TODO: mapFixtureEuro (fixture: Events.Fixture<StageEuro, Domain.UnconfirmedEuro, Domain.MatchEventFootball>) =...
    // TODO: mapFixtureFifa (fixture: Events.Fixture<StageFifa, Domain.UnconfirmedFifa, Domain.MatchEventFootball>) =...
    // TODO: mapFixtureFifaV2 (fixture: Events.Fixture<StageFifa, Domain.UnconfirmedFifaV2, Domain.MatchEventFootball>) =...
    // TODO: mapFixtureRwc (fixture: Events.Fixture<StageRwc, Domain.UnconfirmedRwc, Domain.MatchEventRugby>) =...

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
