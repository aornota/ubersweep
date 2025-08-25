namespace Aornota.Ubersweep.Server.Migration

open Aornota.Ubersweep.Migration
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Migration.Events
open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open Microsoft.Extensions.Configuration
open Serilog
open System
open System.IO

type private UserMapper(userLists: (Guid * User' * Rvn) list list) =
    let rec mapUsers (users: (Guid * User' * Rvn) list) (map: Map<string, Guid * User' * Rvn>) =
        match users with
        | (guid, user, rvn) :: t ->
            if map |> Map.containsKey user.UserName then
                mapUsers t (map |> Map.remove user.UserName |> Map.add user.UserName (guid, user, rvn))
            else
                mapUsers t (map |> Map.add user.UserName (guid, user, rvn))
        | [] -> map

    let userMap =
        mapUsers (userLists |> List.collect id) Map.empty<string, Guid * User' * Rvn>

    member _.Users() = userMap |> Map.values |> List.ofSeq

    member _.MapperFor(sourceUserMap: Map<UserId', string>) : MapUserId =
        fun userId ->
            if userId = UserId'.UserId(Guid AgentUser.agentUserGuid) then
                UserId.FromGuid(Guid AgentUser.agentUserGuid)
            else if sourceUserMap |> Map.containsKey userId then
                let userName = sourceUserMap[userId]

                if userMap |> Map.containsKey userName then
                    let (guid, _, _) = userMap[userName]
                    UserId.FromGuid guid
                else
                    failwith $"Unable to map {userName}"
            else
                failwith $"Unable to map {userId}"

type private PartitionHelper<'group, 'stage, 'unconfirmed, 'playerType, 'matchEvent, 'legacyUnconfirmed, 'legacyMatchEvent>
    (
        root: string,
        partitionName: PartitionName,
        getPartition: string * ILogger -> Partition<'group, 'stage, 'legacyUnconfirmed, 'playerType, 'legacyMatchEvent>,
        mapFixture:
            Fixture'<'stage, 'legacyUnconfirmed, 'legacyMatchEvent> -> Fixture<'stage, 'unconfirmed, 'matchEvent>,
        mapSquad: Squad'<'group, 'playerType> -> Squad<'group, 'playerType>,
        persistenceFactory: IPersistenceFactory,
        logger
    ) =
    let logger =
        SourcedLogger.Create<PartitionHelper<_, _, _, _, _, _, _>>(partitionName, logger)

    let partition = getPartition (Path.Combine(root, partitionName), logger)

    member private _.Check<'state, 'event when 'state :> IState<'state, 'event>>() = asyncResult {
        logger.Debug("Checking {type}s for {partitionName}...", sanitize typeof<'event>, partitionName)
        let reader = persistenceFactory.GetReader<'state, 'event>(Some partitionName)
        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length = 0 then
                logger.Debug(
                    "...can migrate as {type}s do not already exist for {partitionName}",
                    sanitize typeof<'event>,
                    partitionName
                )

                Ok()
            else
                logger.Error(
                    "...cannot migrate as {type}s already exist for {partitionName}",
                    sanitize typeof<'event>,
                    partitionName
                )

                Error []

        return ()
    }

    member this.CheckAll() = asyncResult {
        logger.Debug("Checking {partitionName}...", partitionName)

        let! _ = this.Check<Draft, DraftEvent>()
        let! _ = this.Check<Fixture<'stage, 'unconfirmed, 'matchEvent>, FixtureEvent<'matchEvent>>()
        let! _ = this.Check<Post, PostEvent>()
        let! _ = this.Check<Squad<'group, 'playerType>, SquadEvent<'playerType>>()
        let! _ = this.Check<UserDraft, UserDraftEvent>()

        return ()
    }

    member _.ReadUsers() = partition.ReadUsers()

    member _.MigrateAsync(mapUserId: MapUserId) = asyncResult {
        logger.Debug("Migrating {Draft}s for {partitionName}...", nameof Draft, partitionName)
        let! drafts = partition.ReadDrafts()
        let writer = persistenceFactory.GetWriter<Draft, DraftEvent>(Some partitionName)

        let! writeDraftsResults =
            drafts
            |> List.map (fun (guid, _, draft, rvn) ->
                writer.CreateFromSnapshotAsync(
                    guid,
                    rvn,
                    (mapDraft (draft, mapUserId) :> IState<Draft, DraftEvent>).SnapshotJson
                ))
            |> Async.Parallel

        let! _ =
            match writeDraftsResults |> List.ofArray |> List.sequenceResultA with
            | Ok _ ->
                logger.Debug("...migrated {Draft}s for {partitionName}", nameof Draft, partitionName)
                Ok()
            | Error errors ->
                logger.Error(
                    "...errors migrating {Draft}s for {partitionName}: {errors}",
                    nameof Draft,
                    partitionName,
                    errors
                )

                Error errors

        logger.Debug("Migrating {Fixture}s for {partitionName}...", nameof Fixture, partitionName)
        let! fixtures = partition.ReadFixtures()

        let writer =
            persistenceFactory.GetWriter<
                Fixture<'stage, Unconfirmed<'stage, 'group>, 'matchEvent>,
                FixtureEvent<'matchEvent>
             >(
                Some partitionName
            )

        let! writeFixturesResults =
            fixtures
            |> List.map (fun (guid, _, fixture, rvn) ->
                writer.CreateFromSnapshotAsync(
                    guid,
                    rvn,
                    (mapFixture fixture
                    :> IState<Fixture<'stage, 'unconfirmed, 'matchEvent>, FixtureEvent<'matchEvent>>)
                        .SnapshotJson
                ))
            |> Async.Parallel

        let! _ =
            match writeFixturesResults |> List.ofArray |> List.sequenceResultA with
            | Ok _ ->
                logger.Debug("...migrated {Fixture}s for {partitionName}", nameof Fixture, partitionName)
                Ok()
            | Error errors ->
                logger.Error(
                    "...errors migrating {Fixture}s for {partitionName}: {errors}",
                    nameof Fixture,
                    partitionName,
                    errors
                )

                Error errors

        logger.Debug("Migrating {Post}s for {partitionName}...", nameof Post, partitionName)
        let! posts = partition.ReadPosts()
        let writer = persistenceFactory.GetWriter<Post, PostEvent>(Some partitionName)

        let! writePostsResults =
            posts
            |> List.map (fun (guid, _, post, rvn) ->
                writer.CreateFromSnapshotAsync(
                    guid,
                    rvn,
                    (mapPost (post, mapUserId) :> IState<Post, PostEvent>).SnapshotJson
                ))
            |> Async.Parallel

        let! _ =
            match writePostsResults |> List.ofArray |> List.sequenceResultA with
            | Ok _ ->
                logger.Debug("...migrated {Post}s for {partitionName}", nameof Post, partitionName)
                Ok()
            | Error errors ->
                logger.Error(
                    "...errors migrating {Post}s for {partitionName}: {errors}",
                    nameof Post,
                    partitionName,
                    errors
                )

                Error errors

        logger.Debug("Migrating {Squad}s for {partitionName}...", nameof Squad, partitionName)
        let! squads = partition.ReadSquads()

        let writer =
            persistenceFactory.GetWriter<Squad<'group, 'playerType>, SquadEvent<'playerType>>(Some partitionName)

        let! writeSquadsResults =
            squads
            |> List.map (fun (guid, _, squad, rvn) ->
                writer.CreateFromSnapshotAsync(
                    guid,
                    rvn,
                    (mapSquad squad :> IState<Squad<'group, 'playerType>, SquadEvent<'playerType>>)
                        .SnapshotJson
                ))
            |> Async.Parallel

        let! _ =
            match writeSquadsResults |> List.ofArray |> List.sequenceResultA with
            | Ok _ ->
                logger.Debug("...migrated {Squad}s for {partitionName}", nameof Squad, partitionName)
                Ok()
            | Error errors ->
                logger.Error(
                    "...errors migrating {Squad}s for {partitionName}: {errors}",
                    nameof Squad,
                    partitionName,
                    errors
                )

                Error errors

        logger.Debug("Migrating {UserDraft}s for {partitionName}...", nameof UserDraft, partitionName)
        let! userDrafts = partition.ReadUserDrafts()

        let writer =
            persistenceFactory.GetWriter<UserDraft, UserDraftEvent>(Some partitionName)

        let! writeUserDraftsResults =
            userDrafts
            |> List.map (fun (guid, _, userDraft, rvn) ->
                writer.CreateFromSnapshotAsync(
                    guid,
                    rvn,
                    (mapUserDraft (userDraft, mapUserId) :> IState<UserDraft, UserDraftEvent>)
                        .SnapshotJson
                ))
            |> Async.Parallel

        let! _ =
            match writeUserDraftsResults |> List.ofArray |> List.sequenceResultA with
            | Ok _ ->
                logger.Debug("...migrated {UserDraft}s for {partitionName}", nameof UserDraft, partitionName)
                Ok()
            | Error errors ->
                logger.Error(
                    "...errors migrating {UserDraft}s for {partitionName}: {errors}",
                    nameof UserDraft,
                    partitionName,
                    errors
                )

                Error errors

        return ()
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

    let writeUsersAsync (users: (Guid * User' * Rvn) list) = asyncResult {
        logger.Debug("Writing {Users}s...", nameof User)
        let writer = persistenceFactory.GetWriter<User, UserEvent> None

        let! writeUsersResults =
            users
            |> List.map (fun (guid, user, rvn) ->
                writer.CreateFromSnapshotAsync(guid, rvn, (mapUser user :> IState<User, UserEvent>).SnapshotJson))
            |> Async.Parallel

        let! _ =
            match writeUsersResults |> List.ofArray |> List.sequenceResultA with
            | Ok _ ->
                logger.Debug("...{Users}s written", nameof User)
                Ok()
            | Error errors ->
                logger.Error("...errors writing {User}s: {errors}", nameof User, errors)

                Error errors

        return ()
    }

    member _.MigrateAsync() = asyncResult {
        let mapUsers (users: (Guid * Event<UserEvent'> list * User' * Rvn) list) =
            users |> List.map (fun (guid, _, user, rvn) -> guid, user, rvn)

        let sourceUserMap (users: (Guid * Event<UserEvent'> list * User' * Rvn) list) =
            users
            |> List.map (fun (guid, _, user, _) -> UserId'.UserId guid, user.UserName)
            |> Map.ofList

        let! _ =
            if canMigrate then
                logger.Debug("Checking {User}s...", nameof User)
                Ok()
            else
                logger.Debug "Skipping migration"
                Error []

        let reader = persistenceFactory.GetReader<User, UserEvent> None
        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length = 0 then
                logger.Debug("...can migrate as {User}s do not already exist", nameof User)
                Ok()
            else
                logger.Error("...cannot migrate as {User}s already exist", nameof User)
                Error []

        // TODO-MIGRATION: Stop hard-coding partition names...

        let helperFifa2018 =
            PartitionHelper<
                GroupAToH,
                StageFifa,
                Unconfirmed<StageFifa, GroupAToH>,
                PlayerTypeFootball,
                MatchEventFootball,
                UnconfirmedFifa',
                MatchEventFootball'
             >(
                root,
                "2018-fifa",
                Partition.fifa2018,
                mapFixtureFifa,
                mapSquadFifa,
                persistenceFactory,
                logger
            )

        let helperRwc2019 =
            PartitionHelper<
                GroupAToD,
                StageRwc,
                Unconfirmed<StageRwc, GroupAToD>,
                PlayerTypeRugby,
                MatchEventRugby,
                UnconfirmedRwc',
                MatchEventRugby'
             >(
                root,
                "2019-rwc",
                Partition.rwc2019,
                mapFixtureRwc,
                mapSquadRwc,
                persistenceFactory,
                logger
            )

        let helperEuro2020 =
            PartitionHelper<
                GroupAToF,
                StageEuro,
                UnconfirmedEuro,
                PlayerTypeFootball,
                MatchEventFootball,
                UnconfirmedEuro',
                MatchEventFootball'
             >(
                root,
                "2020-euro",
                Partition.euro2020,
                mapFixtureEuro,
                mapSquadEuro,
                persistenceFactory,
                logger
            )

        let helperFifa2022 =
            PartitionHelper<
                GroupAToH,
                StageFifa,
                Unconfirmed<StageFifa, GroupAToH>,
                PlayerTypeFootball,
                MatchEventFootball,
                UnconfirmedFifaV2',
                MatchEventFootball'
             >(
                root,
                "2022-fifa",
                Partition.fifa2022,
                mapFixtureFifaV2,
                mapSquadFifa,
                persistenceFactory,
                logger
            )

        let helperRwc2023 =
            PartitionHelper<
                GroupAToD,
                StageRwc,
                Unconfirmed<StageRwc, GroupAToD>,
                PlayerTypeRugby,
                MatchEventRugby,
                UnconfirmedRwc',
                MatchEventRugby'
             >(
                root,
                "2023-rwc",
                Partition.rwc2023,
                mapFixtureRwc,
                mapSquadRwc,
                persistenceFactory,
                logger
            )

        let helperEuro2024 =
            PartitionHelper<
                GroupAToF,
                StageEuro,
                UnconfirmedEuro,
                PlayerTypeFootball,
                MatchEventFootball,
                UnconfirmedEuro',
                MatchEventFootball'
             >(
                root,
                "2024-euro",
                Partition.euro2024,
                mapFixtureEuro,
                mapSquadEuro,
                persistenceFactory,
                logger
            )

        let! _ = helperFifa2018.CheckAll()
        let! _ = helperRwc2019.CheckAll()
        let! _ = helperEuro2020.CheckAll()
        let! _ = helperFifa2022.CheckAll()
        let! _ = helperRwc2023.CheckAll()
        let! _ = helperEuro2024.CheckAll()

        let! usersFifa2018 = helperFifa2018.ReadUsers()
        let! usersRwc2019 = helperRwc2019.ReadUsers()
        let! usersEuro2021 = helperEuro2020.ReadUsers()
        let! usersFifa2022 = helperFifa2022.ReadUsers()
        let! usersRwc2023 = helperRwc2023.ReadUsers()
        let! usersEuro2024 = helperEuro2024.ReadUsers()

        let userLists =
            [
                usersFifa2018
                usersRwc2019
                usersEuro2021
                usersFifa2022
                usersRwc2023
                usersEuro2024
            ]
            |> List.map mapUsers

        let userMapper = UserMapper userLists

        let! _ = helperFifa2018.MigrateAsync(userMapper.MapperFor(sourceUserMap usersFifa2018))
        let! _ = helperRwc2019.MigrateAsync(userMapper.MapperFor(sourceUserMap usersRwc2019))
        let! _ = helperEuro2020.MigrateAsync(userMapper.MapperFor(sourceUserMap usersEuro2021))
        let! _ = helperFifa2022.MigrateAsync(userMapper.MapperFor(sourceUserMap usersFifa2022))
        let! _ = helperRwc2023.MigrateAsync(userMapper.MapperFor(sourceUserMap usersRwc2023))
        let! _ = helperEuro2024.MigrateAsync(userMapper.MapperFor(sourceUserMap usersEuro2024))

        let! _ = writeUsersAsync (userMapper.Users())

        return ()
    }
