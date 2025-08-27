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
            if sourceUserMap |> Map.containsKey userId then
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

    member private _.CheckAsync<'state, 'event when 'state :> IState<'state, 'event>>() = asyncResult {
        logger.Debug("...checking {type}s for {partitionName}...", sanitize typeof<'event>, partitionName)
        let reader = persistenceFactory.GetReader<'state, 'event>(Some partitionName)

        let! all = reader.ReadAllAsync()

        return!
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

                Error $"Cannot migrate as {sanitize typeof<'event>}s exist for {partitionName}"
    }

    member this.CheckAllAsync() = asyncResult {
        logger.Debug("Checking {partitionName}...", partitionName)

        let! _ = this.CheckAsync<Draft, DraftEvent>()
        let! _ = this.CheckAsync<Fixture<'stage, 'unconfirmed, 'matchEvent>, FixtureEvent<'matchEvent>>()
        let! _ = this.CheckAsync<Post, PostEvent>()
        let! _ = this.CheckAsync<Squad<'group, 'playerType>, SquadEvent<'playerType>>()
        let! _ = this.CheckAsync<UserDraft, UserDraftEvent>()

        return! Ok()
    }

    member _.ReadUsersAsync() = partition.ReadUsersAsync()

    member _.MigrateAsync(mapUserId: MapUserId) = asyncResult {
        logger.Debug("Migrating {partitionName}...", partitionName)

        logger.Debug("...migrating {Draft}s for {partitionName}...", nameof Draft, partitionName)
        let! drafts = partition.ReadDraftsAsync()
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

        let! _ = // combine Draft results and log
            match
                writeDraftsResults
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when writing {nameof Draft}s: {errors}")
            with
            | Ok _ ->
                logger.Debug("...migrated {Draft}s for {partitionName}", nameof Draft, partitionName)
                Ok()
            | Error error ->
                logger.Error(
                    "...error when migrating {Draft}s for {partitionName}: {error}",
                    nameof Draft,
                    partitionName,
                    error
                )

                Error $"Error when migrating {nameof Draft}s for {partitionName}: {error}"

        logger.Debug("...migrating {Fixture}s for {partitionName}...", nameof Fixture, partitionName)
        let! fixtures = partition.ReadFixturesAsync()

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

        let! _ = // combine Fixture results and log
            match
                writeFixturesResults
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when writing {nameof Fixture}s: {errors}")
            with
            | Ok _ ->
                logger.Debug("...migrated {Fixture}s for {partitionName}", nameof Fixture, partitionName)
                Ok()
            | Error error ->
                logger.Error(
                    "...error when migrating {Fixture}s for {partitionName}: {error}",
                    nameof Fixture,
                    partitionName,
                    error
                )

                Error $"Error when migrating {nameof Fixture}s for {partitionName}: {error}"

        logger.Debug("...migrating {Post}s for {partitionName}...", nameof Post, partitionName)
        let! posts = partition.ReadPostsAsync()
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

        let! _ = // combine Post results and log
            match
                writePostsResults
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when writing {nameof Post}s: {errors}")
            with
            | Ok _ ->
                logger.Debug("...migrated {DrPostaft}s for {partitionName}", nameof Post, partitionName)
                Ok()
            | Error error ->
                logger.Error(
                    "...error when migrating {Post}s for {partitionName}: {error}",
                    nameof Post,
                    partitionName,
                    error
                )

                Error $"Error when migrating {nameof Post}s for {partitionName}: {error}"

        logger.Debug("...migrating {Squad}s for {partitionName}...", nameof Squad, partitionName)
        let! squads = partition.ReadSquadsAsync()

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

        let! _ = // combine Squad results and log
            match
                writeSquadsResults
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when writing {nameof Squad}s: {errors}")
            with
            | Ok _ ->
                logger.Debug("...migrated {Squad}s for {partitionName}", nameof Squad, partitionName)
                Ok()
            | Error error ->
                logger.Error(
                    "...error when migrating {Squad}s for {partitionName}: {error}",
                    nameof Squad,
                    partitionName,
                    error
                )

                Error $"Error when migrating {nameof Squad}s for {partitionName}: {error}"

        logger.Debug("...migrating {UserDraft}s for {partitionName}...", nameof UserDraft, partitionName)
        let! userDrafts = partition.ReadUserDraftsAsync()

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

        let! _ = // combine UserDraft results and log
            match
                writeUserDraftsResults
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when writing {nameof UserDraft}s: {errors}")
            with
            | Ok _ ->
                logger.Debug("...migrated {UserDraft}s for {partitionName}", nameof UserDraft, partitionName)
                Ok()
            | Error error ->
                logger.Error(
                    "...error when migrating {UserDraft}s for {partitionName}: {error}",
                    nameof Draft,
                    partitionName,
                    error
                )

                Error $"Error when migrating {nameof UserDraft}s for {partitionName}: {error}"

        return! Ok()
    }

type Migration(config: IConfiguration, persistenceFactory: IPersistenceFactory, logger) =
    [<Literal>]
    let migrateOnStartUpKey = "MigrateOnStartUp"

    [<Literal>]
    let rootKey = "Root"

    [<Literal>]
    let defaultMigrateOnStartUp = false

    let defaultRoot = String.Empty

    let configuredOrDefault isConfigured =
        if isConfigured then "configured" else "default"

    let logger = SourcedLogger.Create<Migration> logger

    do logger.Verbose "Reading configuration"

    let migrateOnStartUp, isConfiguredMigrateOnStartUp =
        try
            let key = $"{nameof Migration}:{migrateOnStartUpKey}"
            let migrateOnStartUp = config[key]

            if String.IsNullOrWhiteSpace migrateOnStartUp then
                defaultMigrateOnStartUp, false
            else
                match bool.TryParse migrateOnStartUp with
                | true, migrateOnStartUp -> migrateOnStartUp, true
                | _ ->
                    logger.Warning(
                        "Value {migrateOnStartUp} for {key} configuration setting is invalid (must be an boolean)",
                        migrateOnStartUp,
                        key
                    )

                    defaultMigrateOnStartUp, false
        with _ ->
            defaultMigrateOnStartUp, false

    do // logging
        logger.Information(
            "Using {configuredOrDefault} migrate on startup: {migrateOnStartUp}",
            configuredOrDefault isConfiguredMigrateOnStartUp,
            migrateOnStartUp
        )

    let root, isConfiguredRoot =
        try
            let root = config[$"{nameof Migration}:{rootKey}"]

            if String.IsNullOrWhiteSpace root then
                defaultRoot, false
            else
                root, true
        with _ ->
            defaultRoot, false

    do // logging
        if migrateOnStartUp then
            if isConfiguredRoot then
                logger.Information(
                    "Using {configuredOrDefault} migration root: {root}",
                    configuredOrDefault isConfiguredRoot,
                    root
                )
            else
                logger.Warning("Value for {rootKey} configuration setting is missing", rootKey)

    let writeUsersAsync (users: (Guid * User' * Rvn) list) = asyncResult {
        let writer = persistenceFactory.GetWriter<User, UserEvent> None

        let! writeUsersResults =
            users
            |> List.map (fun (guid, user, rvn) ->
                writer.CreateFromSnapshotAsync(guid, rvn, (mapUser user :> IState<User, UserEvent>).SnapshotJson))
            |> Async.Parallel

        let! _ = // combine User results and log
            match
                writeUsersResults
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when writing {nameof User}s: {errors}")
            with
            | Ok _ ->
                logger.Debug("...{User}s written", nameof User)
                Ok()
            | Error error ->
                logger.Error("...error when writing {User}s: {error}", nameof User, error)

                Error $"Error when wriring {nameof User}s: {error}"

        return ()
    }

    let writeSweepstakesAsync (sweepstakes: Sweepstake list) = asyncResult {
        let writer = persistenceFactory.GetWriter<Sweepstake, SweepstakeEvent> None

        let! writeSweepstakesResults =
            sweepstakes
            |> List.map (fun sweepstake ->
                writer.CreateFromSnapshotAsync(
                    Guid.NewGuid(),
                    Rvn.InitialRvn,
                    (sweepstake :> IState<Sweepstake, SweepstakeEvent>).SnapshotJson
                ))
            |> Async.Parallel

        let! _ = // combine Sweepstake results and log
            match
                writeSweepstakesResults
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when writing {nameof Sweepstake}s: {errors}")
            with
            | Ok _ ->
                logger.Debug("...{Sweepstake}s written", nameof Sweepstake)
                Ok()
            | Error error ->
                logger.Error("...error when writing {Sweepstake}s: {error}", nameof Sweepstake, error)

                Error $"Error when wriring {nameof Sweepstake}s: {error}"

        return ()
    }

    member _.MigrateAsync() = asyncResult {
        let mapUsers (users: (Guid * Event<UserEvent'> list * User' * Rvn) list) =
            users |> List.map (fun (guid, _, user, rvn) -> guid, user, rvn)

        let sourceUserMap (users: (Guid * Event<UserEvent'> list * User' * Rvn) list) =
            users
            |> List.map (fun (guid, _, user, _) -> UserId'.UserId guid, user.UserName)
            |> Map.ofList

        if migrateOnStartUp then
            let! _ = // error if value for root not configured
                if not isConfiguredRoot then
                    logger.Error("Cannot migrate as value for {rootKey} configuration setting is missing", rootKey)

                    Error "Configured to migrate on startup but value for {rootKey} configuration setting is missing"
                else
                    Ok()

            logger.Debug "Starting migration..."

            logger.Debug("...checking {User}s...", nameof User)
            let reader = persistenceFactory.GetReader<User, UserEvent> None
            let! all = reader.ReadAllAsync()

            let! _ = // error if Users already exist
                if all.Length = 0 then
                    logger.Debug("...can migrate as {User}s do not already exist", nameof User)
                    Ok()
                else
                    logger.Error("...cannot migrate as {User}s already exist", nameof User)
                    Error $"Cannot migrate as {nameof User}s already exist"

            logger.Debug("...checking {Sweepstake}s...", nameof Sweepstake)
            let reader = persistenceFactory.GetReader<Sweepstake, SweepstakeEvent> None
            let! all = reader.ReadAllAsync()

            let! _ = // error if Sweepstakes already exist
                if all.Length = 0 then
                    logger.Debug("...can migrate as {Sweepstake}s do not already exist", nameof Sweepstake)
                    Ok()
                else
                    logger.Error("...cannot migrate as {Sweepstake}s already exist", nameof Sweepstake)
                    Error $"Cannot migrate as {nameof Sweepstake}s already exist"

            let fifa2018 = {
                SweepstakeCommon = {
                    SweepstakeType = Fifa
                    Year = 2018u
                    Description = "The world-famous World Cup 2018 sweepstake"
                    Logo =
                        "https://github.com/aornota/sweepstake-2018/blob/master/src/resources/images/sweepstake-2018-24x24.png"
                    MaxPlayersPerSquad = 23u
                    Status = Archived
                }
            }

            let rwc2019 = {
                SweepstakeCommon = {
                    SweepstakeType = Rwc
                    Year = 2019u
                    Description = "The world-famous Rugby World Cup 2019 sweepstake"
                    Logo =
                        "https://github.com/aornota/sweepstake-2019/blob/master/src/ui/public/sweepstake-2019-24x24.png"
                    MaxPlayersPerSquad = 31u
                    Status = Archived
                }
            }

            let euro2020 = {
                SweepstakeCommon = {
                    SweepstakeType = Euro
                    Year = 2020u
                    Description = "The world-famous Euro 2020 sweepstake"
                    Logo =
                        "https://github.com/aornota/sweepstake-2021/blob/master/src/ui/public/sweepstake-2021-24x24.png"
                    MaxPlayersPerSquad = 26u
                    Status = Archived
                }
            }

            let fifa2022 = {
                SweepstakeCommon = {
                    SweepstakeType = Fifa
                    Year = 2022u
                    Description = "The world-famous World Cup 2022 sweepstake"
                    Logo =
                        "https://github.com/aornota/sweepstake-2022/blob/main/src/ui/public/sweepstake-2022-24x24.png"
                    MaxPlayersPerSquad = 26u
                    Status = Archived
                }
            }

            let rwc2023 = {
                SweepstakeCommon = {
                    SweepstakeType = Rwc
                    Year = 2023u
                    Description = "The world-famous Rugby World Cup 2023 sweepstake"
                    Logo =
                        "https://github.com/aornota/sweepstake-2023/blob/main/src/ui/public/sweepstake-2023-24x24.png"
                    MaxPlayersPerSquad = 33u
                    Status = Archived
                }
            }

            let euro2024 = {
                SweepstakeCommon = {
                    SweepstakeType = Euro
                    Year = 2024u
                    Description = "The world-famous Euro 2024 sweepstake"
                    Logo =
                        "https://github.com/aornota/sweepstake-2024/blob/main/src/ui/public/sweepstake-2024-24x24.png"
                    MaxPlayersPerSquad = 26u
                    Status = Archived
                }
            }

            logger.Debug "...checking partitions..."

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
                    fifa2018.SweepstakeCommon.PartitionName,
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
                    rwc2019.SweepstakeCommon.PartitionName,
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
                    euro2020.SweepstakeCommon.PartitionName,
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
                    fifa2022.SweepstakeCommon.PartitionName,
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
                    rwc2023.SweepstakeCommon.PartitionName,
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
                    euro2024.SweepstakeCommon.PartitionName,
                    Partition.euro2024,
                    mapFixtureEuro,
                    mapSquadEuro,
                    persistenceFactory,
                    logger
                )

            let! _ = helperFifa2018.CheckAllAsync()
            let! _ = helperRwc2019.CheckAllAsync()
            let! _ = helperEuro2020.CheckAllAsync()
            let! _ = helperFifa2022.CheckAllAsync()
            let! _ = helperRwc2023.CheckAllAsync()
            let! _ = helperEuro2024.CheckAllAsync()

            logger.Debug("...mapping {User}s...", nameof User)

            let! usersFifa2018 = helperFifa2018.ReadUsersAsync()
            let! usersRwc2019 = helperRwc2019.ReadUsersAsync()
            let! usersEuro2021 = helperEuro2020.ReadUsersAsync()
            let! usersFifa2022 = helperFifa2022.ReadUsersAsync()
            let! usersRwc2023 = helperRwc2023.ReadUsersAsync()
            let! usersEuro2024 = helperEuro2024.ReadUsersAsync()

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

            logger.Debug "...migrating partitions..."

            let! _ = helperFifa2018.MigrateAsync(userMapper.MapperFor(sourceUserMap usersFifa2018))
            let! _ = helperRwc2019.MigrateAsync(userMapper.MapperFor(sourceUserMap usersRwc2019))
            let! _ = helperEuro2020.MigrateAsync(userMapper.MapperFor(sourceUserMap usersEuro2021))
            let! _ = helperFifa2022.MigrateAsync(userMapper.MapperFor(sourceUserMap usersFifa2022))
            let! _ = helperRwc2023.MigrateAsync(userMapper.MapperFor(sourceUserMap usersRwc2023))
            let! _ = helperEuro2024.MigrateAsync(userMapper.MapperFor(sourceUserMap usersEuro2024))

            logger.Debug("...writing {User}s...", nameof User)
            let! _ = writeUsersAsync (userMapper.Users())

            logger.Debug("...writing {Sweepstake}s...", nameof Sweepstake)
            let! _ = writeSweepstakesAsync [ fifa2018; rwc2019; euro2020; fifa2022; rwc2023; euro2024 ]

            logger.Debug "...completed migration"

            return! Ok()
        else
            logger.Debug "Not configured to migrate on startup"
            return! Ok()
    }
