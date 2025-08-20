namespace Aornota.Ubersweep.Server.Migration

open Aornota.Ubersweep.Migration
open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open Microsoft.Extensions.Configuration
open System
open System.IO

module Migration =
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

    // TODO: Map UserIds properly...
    let private mapDraftEvents (events: Event<Events.DraftEvent> list, userMapper) =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    // TODO: Map UserIds properly...
    let private mapFixtureEventsEuro
        (
            events: Event<Events.FixtureEvent<Domain.StageEuro, Domain.UnconfirmedEuro, Domain.MatchEventFootball>> list,
            userMapper
        ) =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    // TODO: Map UserIds properly...
    let private mapFixtureEventsFifa
        (
            events: Event<Events.FixtureEvent<Domain.StageFifa, Domain.UnconfirmedFifa, Domain.MatchEventFootball>> list,
            userMapper
        ) =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    // TODO: Map UserIds properly...
    let private mapFixtureEventsFifaV2
        (
            events:
                Event<Events.FixtureEvent<Domain.StageFifa, Domain.UnconfirmedFifaV2, Domain.MatchEventFootball>> list,
            userMapper
        ) =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    let private mapFixtureEventsRwc
        (
            events: Event<Events.FixtureEvent<Domain.StageRwc, Domain.UnconfirmedRwc, Domain.MatchEventRugby>> list,
            userMapper
        ) =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    // TODO: Map UserIds properly...
    let private mapPostEvents (events: Event<Events.NewsEvent> list, userMapper) =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    // TODO: Map UserIds properly...
    let private mapSquadEventsEuro
        (events: Event<Events.SquadEvent<Domain.Group6, Domain.PlayerTypeFootball>> list, userMapper)
        =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    // TODO: Map UserIds properly...
    let private mapSquadEventsFifa
        (events: Event<Events.SquadEvent<Domain.Group8, Domain.PlayerTypeFootball>> list, userMapper)
        =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    // TODO: Map UserIds properly...
    let private mapSquadEventsRwc
        (events: Event<Events.SquadEvent<Domain.Group4, Domain.PlayerTypeRugby>> list, userMapper)
        =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    // TODO: Map UserIds properly...
    let private mapUserEvents (events: Event<Events.UserEvent> list, userMapper) =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

        events
        |> List.map (fun event ->
            let mappedEvent =
                match event.Event with
                | Events.UserCreated(_,
                                     Events.UserName userName,
                                     Events.Salt passwordSalt,
                                     Events.Hash passwordHash,
                                     userType) ->
                    UserCreated(userName, passwordSalt, passwordSalt, mapUserType userType) :> IEvent
                | Events.PasswordChanged(_, Events.Salt passwordSalt, Events.Hash passwordHash) ->
                    PasswordChanged(passwordSalt, passwordHash)
                | Events.PasswordReset(_, Events.Salt passwordSalt, Events.Hash passwordHash) ->
                    UserEvent.PasswordReset(passwordSalt, passwordHash)
                | Events.UserTypeChanged(_, userType) -> UserTypeChanged(mapUserType userType)

            event.Rvn, event.TimestampUtc, mappedEvent, mapUserId event.AuditUserId)

    // TODO: Map UserIds properly...
    let private mapUserDraftEvents (events: Event<Events.UserDraftEvent> list, userMapper) =
        let mapUserId (Domain.UserId guid) = UserId.FromGuid guid // TEMP

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

    let private mapUser (user: Events.User) =
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

type Migration(config: IConfiguration, persistenceFactory: IPersistenceFactory, logger) =
    [<Literal>]
    let migrateOnStartUpKey = "Migration:MigrateOnStartUp"

    [<Literal>]
    let rootKey = "Migration:Root"

    [<Literal>]
    let defaultMigrateOnStartUp = false

    let defaultRoot = String.Empty

    let configuredOrDefault isConfigured =
        if isConfigured then "configured" else "default"

    let logger = SourcedLogger.Create<Migration> logger

    do logger.Verbose "Reading configuration"

    let migrateOnStartUp, isConfiguredMigrateOnStartUp =
        try
            let migrateOnStartUp = config[migrateOnStartUpKey]

            if String.IsNullOrWhiteSpace migrateOnStartUp then
                defaultMigrateOnStartUp, false
            else
                match bool.TryParse migrateOnStartUp with
                | true, migrateOnStartUp -> migrateOnStartUp, true
                | _ ->
                    logger.Warning(
                        "Value {migrateOnStartUp} for {migrateOnStartUpKey} configuration setting is invalid (must be an boolean)",
                        migrateOnStartUp,
                        migrateOnStartUpKey
                    )

                    defaultMigrateOnStartUp, false
        with _ ->
            defaultMigrateOnStartUp, false

    do
        logger.Information(
            "Using {configuredOrDefault} migrate on startup: {migrateOnStartUp}",
            configuredOrDefault isConfiguredMigrateOnStartUp,
            migrateOnStartUp
        )

    let root, isConfiguredRoot =
        try
            let root = config[rootKey]

            if String.IsNullOrWhiteSpace root then
                defaultRoot, false
            else
                root, true
        with _ ->
            defaultRoot, false

    do
        if migrateOnStartUp then
            if isConfiguredRoot then
                logger.Information(
                    "Using {configuredOrDefault} migration root: {root}",
                    configuredOrDefault isConfiguredRoot,
                    root
                )
            else
                logger.Error("Value for {rootKey} configuration setting is missing", rootKey)

    let canMigrate = migrateOnStartUp && isConfiguredRoot

    member private _.CheckPartitionAsync<'group, 'stage, 'unconfirmed, 'playerType, 'matchEvent>(name: PartitionName) = asyncResult {
        logger.Information("Checking partition {name}...", name)
        let reader = persistenceFactory.GetReader<Draft, DraftEvent>(Some name)
        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {draft}s already exist", nameof Draft)
                Error []
            else
                Ok()

        let reader =
            persistenceFactory.GetReader<Fixture<'stage, 'unconfirmed, 'matchEvent>, FixtureEvent<'matchEvent>>(
                Some name
            )

        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {fixture}s already exist", nameof Fixture)
                Error []
            else
                Ok()

        let reader = persistenceFactory.GetReader<Post, PostEvent>(Some name)

        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {post}s already exist", nameof Post)
                Error []
            else
                Ok()

        let reader =
            persistenceFactory.GetReader<Squad<'group, 'playerType>, SquadEvent<'playerType>>(Some name)

        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {squad}s already exist", nameof Squad)
                Error []
            else
                Ok()

        let reader = persistenceFactory.GetReader<UserDraft, UserDraftEvent>(Some name)

        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {userdraft}s already exist", nameof UserDraft)
                Error []
            else
                Ok()

        ()
    }

    member this.MigrateAsync() = asyncResult {
        let! _ =
            if canMigrate then
                logger.Information "Checking Users..."
                Ok()
            else
                logger.Information "Skipping migration"
                Error []

        let reader = persistenceFactory.GetReader<User, UserEvent> None
        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {user}s already exist", nameof User)
                Error []
            else
                Ok()

        // TODO: Improve / abstract this...

        let! _ =
            this.CheckPartitionAsync<
                GroupAToH,
                StageFifa,
                Unconfirmed<StageFifa, GroupAToH>,
                PlayerTypeFootball,
                MatchEventFootball
             >
                "2018-fifa"

        let! _ =
            this.CheckPartitionAsync<
                GroupAToD,
                StageRwc,
                Unconfirmed<StageRwc, GroupAToD>,
                PlayerTypeRugby,
                MatchEventRugby
             >
                "2019-rwc"

        let! _ =
            this.CheckPartitionAsync<
                GroupAToF,
                StageEuro,
                Unconfirmed<StageEuro, GroupAToF>,
                PlayerTypeFootball,
                MatchEventFootball
             >
                "2021-euro"

        let! _ =
            this.CheckPartitionAsync<
                GroupAToH,
                StageFifa,
                Unconfirmed<StageFifa, GroupAToH>,
                PlayerTypeFootball,
                MatchEventFootball
             >
                "2022-fifa"

        let! _ =
            this.CheckPartitionAsync<
                GroupAToD,
                StageRwc,
                Unconfirmed<StageRwc, GroupAToD>,
                PlayerTypeRugby,
                MatchEventRugby
             >
                "2023-rwc"

        let! _ =
            this.CheckPartitionAsync<
                GroupAToF,
                StageEuro,
                Unconfirmed<StageEuro, GroupAToF>,
                PlayerTypeFootball,
                MatchEventFootball
             >
                "2024-euro"

        let fifa2018 = Partition.fifa2018 (Path.Combine(root, "2018-fifa"), logger)
        let! fifa2018Users = fifa2018.ReadUsers()

        let rwc2019 = Partition.rwc2019 (Path.Combine(root, "2019-rwc"), logger)
        let! rwc2019Users = rwc2019.ReadUsers()

        let euro2021 = Partition.euro2021 (Path.Combine(root, "2021-euro"), logger)
        let! euro2021Users = euro2021.ReadUsers()

        let fifa2022 = Partition.fifa2022 (Path.Combine(root, "2022-fifa"), logger)
        let! fifa2022Users = fifa2022.ReadUsers()

        let rwc2023 = Partition.rwc2023 (Path.Combine(root, "2023-rwc"), logger)
        let! rwc2023Users = rwc2023.ReadUsers()

        let euro2024 = Partition.euro2024 (Path.Combine(root, "2024-euro"), logger)
        let! euro2024Users = euro2024.ReadUsers()

        (* TODO:
            [-- bail if any Sweepstakes?...]
            -- for each "partition":
                - read events and entities...
                - create User mapping...
                - write Users...
                - write mapped events (or migrated snapshots, e.g. for Users?)...*)

        ()
    }
