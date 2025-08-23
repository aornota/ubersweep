namespace Aornota.Ubersweep.Server.Migration

open Aornota.Ubersweep.Migration
open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

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

    let private mapParticipant mapUnconfirmed =
        function
        | Domain.Confirmed squadId -> Confirmed(mapSquadId squadId)
        | Domain.Unconfirmed unconfirmed -> Unconfirmed(mapUnconfirmed unconfirmed)

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

    let mapPostEvents (events: Event<Events.NewsEvent> list, mapUserId: MapUserId) =
        let mapPostType =
            function
            | Domain.Standard -> Standard
            | Domain.MatchResult fixtureId -> MatchResult(mapFixtureId fixtureId)

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.PostCreated(_, userId, postType, messageText, timestamp) ->
                    PostCreated(mapUserId userId, mapPostType postType, messageText, timestamp) :> IEvent
                | Events.PostChanged(_, messageText) -> PostChanged messageText
                | Events.PostRemoved _ -> PostRemoved

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapSquadEventsEuro
        (events: Event<Events.SquadEvent<GroupAToF, PlayerTypeFootball>> list, mapUserId: MapUserId)
        =
        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.SquadCreated(_, Events.SquadName squadName, group, seeding, Events.CoachName coachName) ->
                    SquadCreated(squadName, group, seeding, coachName) :> IEvent
                | Events.PlayerAdded(_, playerId, Events.PlayerName playerName, playerType) ->
                    PlayerAdded(mapPlayerId playerId, playerName, playerType)
                | Events.PlayerNameChanged(_, playerId, Events.PlayerName playerName) ->
                    PlayerNameChanged(mapPlayerId playerId, playerName)
                | Events.PlayerTypeChanged(_, playerId, playerType) ->
                    PlayerTypeChanged(mapPlayerId playerId, playerType)
                | Events.PlayerWithdrawn(_, playerId, dateWithdrawn) ->
                    PlayerWithdrawn(mapPlayerId playerId, dateWithdrawn)
                | Events.SquadEliminated _ -> SquadEliminated

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapSquadEventsFifa
        (events: Event<Events.SquadEvent<GroupAToH, PlayerTypeFootball>> list, mapUserId: MapUserId)
        =
        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.SquadCreated(_, Events.SquadName squadName, group, seeding, Events.CoachName coachName) ->
                    SquadCreated(squadName, group, seeding, coachName) :> IEvent
                | Events.PlayerAdded(_, playerId, Events.PlayerName playerName, playerType) ->
                    PlayerAdded(mapPlayerId playerId, playerName, playerType)
                | Events.PlayerNameChanged(_, playerId, Events.PlayerName playerName) ->
                    PlayerNameChanged(mapPlayerId playerId, playerName)
                | Events.PlayerTypeChanged(_, playerId, playerType) ->
                    PlayerTypeChanged(mapPlayerId playerId, playerType)
                | Events.PlayerWithdrawn(_, playerId, dateWithdrawn) ->
                    PlayerWithdrawn(mapPlayerId playerId, dateWithdrawn)
                | Events.SquadEliminated _ -> SquadEliminated

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapSquadEventsRwc (events: Event<Events.SquadEvent<GroupAToD, PlayerTypeRugby>> list, mapUserId: MapUserId) =
        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.SquadCreated(_, Events.SquadName squadName, group, seeding, Events.CoachName coachName) ->
                    SquadCreated(squadName, group, seeding, coachName) :> IEvent
                | Events.PlayerAdded(_, playerId, Events.PlayerName playerName, playerType) ->
                    PlayerAdded(mapPlayerId playerId, playerName, playerType)
                | Events.PlayerNameChanged(_, playerId, Events.PlayerName playerName) ->
                    PlayerNameChanged(mapPlayerId playerId, playerName)
                | Events.PlayerTypeChanged(_, playerId, playerType) ->
                    PlayerTypeChanged(mapPlayerId playerId, playerType)
                | Events.PlayerWithdrawn(_, playerId, dateWithdrawn) ->
                    PlayerWithdrawn(mapPlayerId playerId, dateWithdrawn)
                | Events.SquadEliminated _ -> SquadEliminated

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapUserDraftEvents (events: Event<Events.UserDraftEvent> list, mapUserId: MapUserId) =
        let mapUserDraftPick =
            function
            | Domain.TeamPick sqaudId -> TeamPick(mapSquadId sqaudId)
            | Domain.PlayerPick(squadId, playerId) -> PlayerPick(mapSquadId squadId, mapPlayerId playerId)

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.UserDraftCreated(_, userId, draftId) ->
                    UserDraftCreated(mapUserId userId, mapDraftId draftId) :> IEvent
                | Events.Drafted(_, userDraftPick) -> Drafted(mapUserDraftPick userDraftPick)
                | Events.Undrafted(_, userDraftPick) -> Undrafted(mapUserDraftPick userDraftPick)
                | Events.PriorityChanged(_, userDraftPick, priorityChange) ->
                    PriorityChanged(mapUserDraftPick userDraftPick, priorityChange)

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapUser (user: Events.User) =

        {
            UserCommon = {
                UserName = user.UserName
                UserType = user.UserType
                MustChangePasswordReason = user.MustChangePasswordReason
            }
            PasswordSalt = user.PasswordSalt
            PasswordHash = user.PasswordHash
        }
