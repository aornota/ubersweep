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

    let private mapGroup4 =
        function
        | Domain.Group4.GroupA -> GroupAToD.GroupA
        | Domain.Group4.GroupB -> GroupAToD.GroupB
        | Domain.Group4.GroupC -> GroupAToD.GroupC
        | Domain.Group4.GroupD -> GroupAToD.GroupD

    let private mapGroup6 =
        function
        | Domain.Group6.GroupA -> GroupAToF.GroupA
        | Domain.Group6.GroupB -> GroupAToF.GroupB
        | Domain.Group6.GroupC -> GroupAToF.GroupC
        | Domain.Group6.GroupD -> GroupAToF.GroupD
        | Domain.Group6.GroupE -> GroupAToF.GroupE
        | Domain.Group6.GroupF -> GroupAToF.GroupF

    let private mapGroup8 =
        function
        | Domain.Group8.GroupA -> GroupAToH.GroupA
        | Domain.Group8.GroupB -> GroupAToH.GroupB
        | Domain.Group8.GroupC -> GroupAToH.GroupC
        | Domain.Group8.GroupD -> GroupAToH.GroupD
        | Domain.Group8.GroupE -> GroupAToH.GroupE
        | Domain.Group8.GroupF -> GroupAToH.GroupF
        | Domain.Group8.GroupG -> GroupAToH.GroupG
        | Domain.Group8.GroupH -> GroupAToH.GroupH

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

    let private mapPlayerTypeFootabll =
        function
        | Domain.Goalkeeper -> Goalkeeper
        | Domain.Defender -> Defender
        | Domain.Midfielder -> Midfielder
        | Domain.Forward -> PlayerTypeFootball.Forward

    let private mapRole =
        function
        | Domain.Home -> Home
        | Domain.Away -> Away

    let private mapSeeding =
        function
        | Domain.Seeding seeding -> Seeding seeding

    let private mapParticipant mapUnconfirmed =
        function
        | Domain.Confirmed squadId -> Confirmed(mapSquadId squadId)
        | Domain.Unconfirmed unconfirmed -> Unconfirmed(mapUnconfirmed unconfirmed)

    let private mapStageFifa =
        function
        | Domain.StageFifa.Group group -> StageFifa.Group(mapGroup8 group)
        | Domain.StageFifa.RoundOf16 matchNumber -> RoundOf16 matchNumber
        | Domain.StageFifa.QuarterFinal quarterFinalOrdinal -> StageFifa.QuarterFinal quarterFinalOrdinal
        | Domain.StageFifa.SemiFinal semiFinalOrdinal -> StageFifa.SemiFinal semiFinalOrdinal
        | Domain.StageFifa.ThirdPlacePlayOff -> ThirdPlacePlayOff
        | Domain.StageFifa.Final -> StageFifa.Final

    let private mapUserType =
        function
        | Domain.SuperUser -> SuperUser
        | Domain.Administrator -> Administrator
        | Domain.Pleb -> Pleb
        | Domain.PersonaNonGrata -> PersonaNonGrata

    let mapDraftEvents (events: Event<Events.DraftEvent> list, mapUserId: MapUserId) =
        let mapDraftType =
            function
            | Domain.Constrained(starts, ends) -> Constrained(starts, ends)
            | Domain.Unconstrained -> Unconstrained

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
                | Events.DraftCreated(_, Domain.DraftOrdinal draftOrdinal, draftType) ->
                    DraftCreated(DraftOrdinal draftOrdinal, mapDraftType draftType) :> IEvent
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
                | Events.Picked(_, Domain.DraftOrdinal draftOrdinal, draftPick, userId, timestamp) ->
                    DraftEvent.Picked(DraftOrdinal draftOrdinal, mapDraftPick draftPick, mapUserId userId, timestamp)
                | Events.DraftProcessed _ -> DraftProcessed
                | Events.DraftFreeSelection _ -> DraftFreeSelection
                | Events.FreePick(_, draftPick, userId, timestamp) ->
                    FreePick(mapDraftPick draftPick, mapUserId userId, timestamp)

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapFixtureEventsEuro
        (
            events: Event<Events.FixtureEvent<Domain.StageEuro, Domain.UnconfirmedEuro, Domain.MatchEventFootball>> list,
            mapUserId: MapUserId
        ) =
        let mapStageEuro =
            function
            | Domain.StageEuro.Group group -> StageEuro.Group(mapGroup6 group)
            | Domain.StageEuro.RoundOf16 matchNumber -> StageEuro.RoundOf16 matchNumber
            | Domain.StageEuro.QuarterFinal quarterFinalOrdinal -> StageEuro.QuarterFinal quarterFinalOrdinal
            | Domain.StageEuro.SemiFinal semiFinalOrdinal -> StageEuro.SemiFinal semiFinalOrdinal
            | Domain.StageEuro.Final -> StageEuro.Final

        let mapUnconfirmedEuro =
            function
            | Domain.UnconfirmedEuro.Winner stage -> UnconfirmedEuro.Winner(mapStageEuro stage)
            | Domain.UnconfirmedEuro.RunnerUp group -> UnconfirmedEuro.RunnerUp(mapGroup6 group)
            | Domain.UnconfirmedEuro.ThirdPlace groups -> UnconfirmedEuro.ThirdPlace(groups |> List.map mapGroup6)

        let mapParticipantEuro = mapParticipant mapUnconfirmedEuro

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.FixtureCreated(_, stage, homeParticipant, awayParticipant, kickOff) ->
                    FixtureCreated(
                        mapStageEuro stage,
                        mapParticipantEuro homeParticipant,
                        mapParticipantEuro awayParticipant,
                        kickOff
                    )
                    :> IEvent
                | Events.ParticipantConfirmed(_, role, squadId) ->
                    ParticipantConfirmed(mapRole role, mapSquadId squadId)
                | Events.MatchEventAdded(_, matchEventId, matchEvent) ->
                    MatchEventAdded(mapMatchEventId matchEventId, mapMatchEventFootball matchEvent)
                | Events.MatchEventRemoved(_, matchEventId) -> MatchEventRemoved(mapMatchEventId matchEventId)
                | Events.FixtureCancelled _ -> FixtureCancelled

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapFixtureEventsFifa
        (
            events: Event<Events.FixtureEvent<Domain.StageFifa, Domain.UnconfirmedFifa, Domain.MatchEventFootball>> list,
            mapUserId: MapUserId
        ) =
        let mapUnconfirmedFifa =
            function
            | Domain.UnconfirmedFifa.Winner stage -> Winner(mapStageFifa stage)
            | Domain.UnconfirmedFifa.RunnerUp group -> RunnerUp(mapGroup8 group)
            | Domain.UnconfirmedFifa.Loser semiFinalOrdinal -> Loser(StageFifa.SemiFinal semiFinalOrdinal)

        let mapParticipantFifa = mapParticipant mapUnconfirmedFifa

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.FixtureCreated(_, stage, homeParticipant, awayParticipant, kickOff) ->
                    FixtureCreated(
                        mapStageFifa stage,
                        mapParticipantFifa homeParticipant,
                        mapParticipantFifa awayParticipant,
                        kickOff
                    )
                    :> IEvent
                | Events.ParticipantConfirmed(_, role, squadId) ->
                    ParticipantConfirmed(mapRole role, mapSquadId squadId)
                | Events.MatchEventAdded(_, matchEventId, matchEvent) ->
                    MatchEventAdded(mapMatchEventId matchEventId, mapMatchEventFootball matchEvent)
                | Events.MatchEventRemoved(_, matchEventId) -> MatchEventRemoved(mapMatchEventId matchEventId)
                | Events.FixtureCancelled _ -> FixtureCancelled

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapFixtureEventsFifaV2
        (
            events:
                Event<Events.FixtureEvent<Domain.StageFifa, Domain.UnconfirmedFifaV2, Domain.MatchEventFootball>> list,
            mapUserId: MapUserId
        ) =
        let mapUnconfirmedFifaV2 =
            function
            | Domain.UnconfirmedFifaV2.Winner stage -> Winner(mapStageFifa stage)
            | Domain.UnconfirmedFifaV2.RunnerUp group -> RunnerUp(mapGroup8 group)
            | Domain.UnconfirmedFifaV2.Loser stage -> Loser(mapStageFifa stage)

        let mapParticipantFifa = mapParticipant mapUnconfirmedFifaV2

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.FixtureCreated(_, stage, homeParticipant, awayParticipant, kickOff) ->
                    FixtureCreated(
                        mapStageFifa stage,
                        mapParticipantFifa homeParticipant,
                        mapParticipantFifa awayParticipant,
                        kickOff
                    )
                    :> IEvent
                | Events.ParticipantConfirmed(_, role, squadId) ->
                    ParticipantConfirmed(mapRole role, mapSquadId squadId)
                | Events.MatchEventAdded(_, matchEventId, matchEvent) ->
                    MatchEventAdded(mapMatchEventId matchEventId, mapMatchEventFootball matchEvent)
                | Events.MatchEventRemoved(_, matchEventId) -> MatchEventRemoved(mapMatchEventId matchEventId)
                | Events.FixtureCancelled _ -> FixtureCancelled

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapFixtureEventsRwc
        (
            events: Event<Events.FixtureEvent<Domain.StageRwc, Domain.UnconfirmedRwc, Domain.MatchEventRugby>> list,
            mapUserId: MapUserId
        ) =
        let mapMatchEventRugby matchEvent =
            let mapKickOutcome =
                function
                | Domain.Successful -> Successful
                | Domain.KickOutcome.Missed -> Missed

            match matchEvent with
            | Domain.Try(squadId, playerId) -> Try(mapSquadId squadId, mapPlayerId playerId)
            | Domain.PenaltyTry squadId -> PenaltyTry(mapSquadId squadId)
            | Domain.PenaltyKick(squadId, playerId, kickOutcome) ->
                PenaltyKick(mapSquadId squadId, mapPlayerId playerId, mapKickOutcome kickOutcome)
            | Domain.Conversion(squadId, playerId, kickOutcome) ->
                Conversion(mapSquadId squadId, mapPlayerId playerId, mapKickOutcome kickOutcome)
            | Domain.DropGoal(squadId, playerId) -> DropGoal(mapSquadId squadId, mapPlayerId playerId)
            | Domain.MatchEventRugby.YellowCard(squadId, playerId) ->
                YellowCard(mapSquadId squadId, mapPlayerId playerId)
            | Domain.MatchEventRugby.RedCard(squadId, playerId) -> RedCard(mapSquadId squadId, mapPlayerId playerId)
            | Domain.MatchEventRugby.ManOfTheMatch(squadId, playerId) ->
                ManOfTheMatch(mapSquadId squadId, mapPlayerId playerId)

        let mapStageRwc =
            function
            | Domain.StageRwc.Group group -> Group(mapGroup4 group)
            | Domain.StageRwc.QuarterFinal quarterFinalOrdinal -> QuarterFinal quarterFinalOrdinal
            | Domain.StageRwc.SemiFinal semiFinalOrdinal -> SemiFinal semiFinalOrdinal
            | Domain.StageRwc.BronzeFinal -> BronzeFinal
            | Domain.StageRwc.Final -> Final

        let mapUnconfirmedRwc =
            function
            | Domain.UnconfirmedRwc.GroupRunnerUp group -> RunnerUp(mapGroup4 group)
            | Domain.UnconfirmedRwc.StageWinner stage -> Winner(mapStageRwc stage)
            | Domain.UnconfirmedRwc.SemiFinalLoser semiFinalOrdinal -> Loser(SemiFinal semiFinalOrdinal)

        let mapParticipantRwc = mapParticipant mapUnconfirmedRwc

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.FixtureCreated(_, stage, homeParticipant, awayParticipant, kickOff) ->
                    FixtureCreated(
                        mapStageRwc stage,
                        mapParticipantRwc homeParticipant,
                        mapParticipantRwc awayParticipant,
                        kickOff
                    )
                    :> IEvent
                | Events.ParticipantConfirmed(_, role, squadId) ->
                    ParticipantConfirmed(mapRole role, mapSquadId squadId)
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
        (events: Event<Events.SquadEvent<Domain.Group6, Domain.PlayerTypeFootball>> list, mapUserId: MapUserId)
        =
        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.SquadCreated(_, Events.SquadName squadName, group, seeding, Events.CoachName coachName) ->
                    SquadCreated(squadName, mapGroup6 group, seeding |> Option.map mapSeeding, coachName) :> IEvent
                | Events.PlayerAdded(_, playerId, Events.PlayerName playerName, playerType) ->
                    PlayerAdded(mapPlayerId playerId, playerName, mapPlayerTypeFootabll playerType)
                | Events.PlayerNameChanged(_, playerId, Events.PlayerName playerName) ->
                    PlayerNameChanged(mapPlayerId playerId, playerName)
                | Events.PlayerTypeChanged(_, playerId, playerType) ->
                    PlayerTypeChanged(mapPlayerId playerId, mapPlayerTypeFootabll playerType)
                | Events.PlayerWithdrawn(_, playerId, dateWithdrawn) ->
                    PlayerWithdrawn(mapPlayerId playerId, dateWithdrawn)
                | Events.SquadEliminated _ -> SquadEliminated

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapSquadEventsFifa
        (events: Event<Events.SquadEvent<Domain.Group8, Domain.PlayerTypeFootball>> list, mapUserId: MapUserId)
        =
        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.SquadCreated(_, Events.SquadName squadName, group, seeding, Events.CoachName coachName) ->
                    SquadCreated(squadName, mapGroup8 group, seeding |> Option.map mapSeeding, coachName) :> IEvent
                | Events.PlayerAdded(_, playerId, Events.PlayerName playerName, playerType) ->
                    PlayerAdded(mapPlayerId playerId, playerName, mapPlayerTypeFootabll playerType)
                | Events.PlayerNameChanged(_, playerId, Events.PlayerName playerName) ->
                    PlayerNameChanged(mapPlayerId playerId, playerName)
                | Events.PlayerTypeChanged(_, playerId, playerType) ->
                    PlayerTypeChanged(mapPlayerId playerId, mapPlayerTypeFootabll playerType)
                | Events.PlayerWithdrawn(_, playerId, dateWithdrawn) ->
                    PlayerWithdrawn(mapPlayerId playerId, dateWithdrawn)
                | Events.SquadEliminated _ -> SquadEliminated

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapSquadEventsRwc
        (events: Event<Events.SquadEvent<Domain.Group4, Domain.PlayerTypeRugby>> list, mapUserId: MapUserId)
        =
        let mapPlayerTypeRugby =
            function
            | Domain.PlayerTypeRugby.Forward -> Forward
            | Domain.Back -> Back

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.SquadCreated(_, Events.SquadName squadName, group, seeding, Events.CoachName coachName) ->
                    SquadCreated(squadName, mapGroup4 group, seeding |> Option.map mapSeeding, coachName) :> IEvent
                | Events.PlayerAdded(_, playerId, Events.PlayerName playerName, playerType) ->
                    PlayerAdded(mapPlayerId playerId, playerName, mapPlayerTypeRugby playerType)
                | Events.PlayerNameChanged(_, playerId, Events.PlayerName playerName) ->
                    PlayerNameChanged(mapPlayerId playerId, playerName)
                | Events.PlayerTypeChanged(_, playerId, playerType) ->
                    PlayerTypeChanged(mapPlayerId playerId, mapPlayerTypeRugby playerType)
                | Events.PlayerWithdrawn(_, playerId, dateWithdrawn) ->
                    PlayerWithdrawn(mapPlayerId playerId, dateWithdrawn)
                | Events.SquadEliminated _ -> SquadEliminated

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapUserDraftEvents (events: Event<Events.UserDraftEvent> list, mapUserId: MapUserId) =
        let mapUserDraftPick =
            function
            | Domain.TeamPick sqaudId -> TeamPick(mapSquadId sqaudId)
            | Domain.PlayerPick(squadId, playerId) -> PlayerPick(mapSquadId squadId, mapPlayerId playerId)

        let mapPriorityChange =
            function
            | Domain.Increase -> Increase
            | Domain.Decrease -> Decrease

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.UserDraftCreated(_, userId, draftId) ->
                    UserDraftCreated(mapUserId userId, mapDraftId draftId) :> IEvent
                | Events.Drafted(_, userDraftPick) -> Drafted(mapUserDraftPick userDraftPick)
                | Events.Undrafted(_, userDraftPick) -> Undrafted(mapUserDraftPick userDraftPick)
                | Events.PriorityChanged(_, userDraftPick, priorityChange) ->
                    PriorityChanged(mapUserDraftPick userDraftPick, mapPriorityChange priorityChange)

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    let mapUser (user: Events.User) =
        let mapMustChangePasswordReason =
            function
            | Domain.FirstSignIn -> FirstSignIn
            | Domain.PasswordReset -> PasswordReset

        {
            UserCommon = {
                UserName = user.UserName
                UserType = mapUserType user.UserType
                MustChangePasswordReason = user.MustChangePasswordReason |> Option.map mapMustChangePasswordReason
            }
            PasswordSalt = user.PasswordSalt
            PasswordHash = user.PasswordHash
        }
