namespace Aornota.Ubersweep.Server.Migration

open Aornota.Ubersweep.Migration
open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open Microsoft.Extensions.Configuration
open Serilog
open System
open System.Collections.Generic
open System.IO

type private UserMapper(userLists: (Guid * Events.User * Rvn) list list) =
    let userDic = Dictionary<string, Guid * Events.User * Rvn>()

    do
        userLists
        |> List.iter (fun users ->
            users
            |> List.iter (fun (guid, user, rvn) ->
                if userDic.ContainsKey user.UserName then
                    userDic[user.UserName] <- guid, user, rvn
                else
                    userDic.Add(user.UserName, (guid, user, rvn))))

    member _.Users() = userDic.Values |> List.ofSeq

    member _.MapperFor(sourceUserDic: Dictionary<Domain.UserId, string>) : MapUserId =
        fun userId ->
            if userId = Domain.UserId(Guid AgentUser.agentUserGuid) then
                UserId.FromGuid(Guid AgentUser.agentUserGuid)
            else if sourceUserDic.ContainsKey userId then
                let userName = sourceUserDic[userId]

                if userDic.ContainsKey userName then
                    let (guid, _, _) = userDic[userName]
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
            Events.Fixture<'stage, 'legacyUnconfirmed, 'legacyMatchEvent> -> Fixture<'stage, 'unconfirmed, 'matchEvent>,
        mapSquad: Events.Squad<'group, 'playerType> -> Squad<'group, 'playerType>,
        persistenceFactory: IPersistenceFactory,
        logger
    ) =
    let logger =
        SourcedLogger.Create<PartitionHelper<_, _, _, _, _, _, _>>(partitionName, logger)

    let partition = getPartition (Path.Combine(root, partitionName), logger)

    member _.Check() = asyncResult {
        logger.Information("Checking...", partitionName)
        let reader = persistenceFactory.GetReader<Draft, DraftEvent>(Some partitionName)
        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {draft}s already exist", nameof Draft)
                Error []
            else
                Ok()

        let reader =
            persistenceFactory.GetReader<Fixture<'stage, 'unconfirmed, 'matchEvent>, FixtureEvent<'matchEvent>>(
                Some partitionName
            )

        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {fixture}s already exist", nameof Fixture)
                Error []
            else
                Ok()

        let reader = persistenceFactory.GetReader<Post, PostEvent>(Some partitionName)

        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {post}s already exist", nameof Post)
                Error []
            else
                Ok()

        let reader =
            persistenceFactory.GetReader<Squad<'group, 'playerType>, SquadEvent<'playerType>>(Some partitionName)

        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {squad}s already exist", nameof Squad)
                Error []
            else
                Ok()

        let reader =
            persistenceFactory.GetReader<UserDraft, UserDraftEvent>(Some partitionName)

        let! all = reader.ReadAllAsync()

        let! _ =
            if all.Length > 0 then
                logger.Error("...cannot migrate as {userdraft}s already exist", nameof UserDraft)
                Error []
            else
                Ok()

        ()
    }

    member _.ReadUsers() = partition.ReadUsers()

    member _.MigrateAsync(mapUserId: MapUserId) = asyncResult {
        // TODO-MIGRATE: What to do about timestamp (since cannot currently pass to IReader.WriteEvent)?...

        let! drafts = partition.ReadDrafts()

        let mappedDrafts =
            drafts
            |> List.map (fun (guid, _, draft, rvn) -> guid, mapDraft (draft, mapUserId), rvn)

        let writer = persistenceFactory.GetWriter<Draft, DraftEvent>(Some partitionName)

        // TODO-MIGRATION: Should not ignore - plus logging?...

        do
            mappedDrafts
            |> List.iter (fun (guid, draft, rvn) ->
                writer.CreateFromSnapshotAsync(guid, rvn, (draft :> IState<Draft, DraftEvent>).SnapshotJson)
                |> Async.RunSynchronously
                |> ignore)

        let! fixtures = partition.ReadFixtures()

        let mappedFixtures =
            fixtures
            |> List.map (fun (guid, _, fixture, rvn) -> guid, mapFixture fixture, rvn)

        let writer =
            persistenceFactory.GetWriter<
                Fixture<'stage, Unconfirmed<'stage, 'group>, 'matchEvent>,
                FixtureEvent<'matchEvent>
             >(
                Some partitionName
            )

        // TODO-MIGRATION: Should not ignore - plus logging?...

        do
            mappedFixtures
            |> List.iter (fun (guid, fixture, rvn) ->
                writer.CreateFromSnapshotAsync(
                    guid,
                    rvn,
                    (fixture :> IState<Fixture<'stage, 'unconfirmed, 'matchEvent>, FixtureEvent<'matchEvent>>)
                        .SnapshotJson
                )
                |> Async.RunSynchronously
                |> ignore)

        let! posts = partition.ReadPosts()

        let mappedPosts =
            posts
            |> List.map (fun (guid, _, post, rvn) -> guid, mapPost (post, mapUserId), rvn)

        let writer = persistenceFactory.GetWriter<Post, PostEvent>(Some partitionName)

        // TODO-MIGRATION: Should not ignore - plus logging?...

        do
            mappedPosts
            |> List.iter (fun (guid, post, rvn) ->
                writer.CreateFromSnapshotAsync(guid, rvn, (post :> IState<Post, PostEvent>).SnapshotJson)
                |> Async.RunSynchronously
                |> ignore)

        let! squads = partition.ReadSquads()

        let mappedSquads =
            squads |> List.map (fun (guid, _, squad, rvn) -> guid, mapSquad squad, rvn)

        let writer =
            persistenceFactory.GetWriter<Squad<'group, 'playerType>, SquadEvent<'playerType>>(Some partitionName)

        do
            mappedSquads
            |> List.iter (fun (guid, squad, rvn) ->
                writer.CreateFromSnapshotAsync(
                    guid,
                    rvn,
                    (squad :> IState<Squad<'group, 'playerType>, SquadEvent<'playerType>>)
                        .SnapshotJson
                )
                |> Async.RunSynchronously
                |> ignore)

        let! userDrafts = partition.ReadUserDrafts()

        let mappedUserDrafts =
            userDrafts
            |> List.map (fun (guid, _, userDraft, rvn) -> guid, mapUserDraft (userDraft, mapUserId), rvn)

        let writer =
            persistenceFactory.GetWriter<UserDraft, UserDraftEvent>(Some partitionName)

        // TODO-MIGRATION: Should not ignore - plus logging?...

        do
            mappedUserDrafts
            |> List.iter (fun (guid, userDraft, rvn) ->
                writer.CreateFromSnapshotAsync(
                    guid,
                    rvn,
                    (userDraft :> IState<UserDraft, UserDraftEvent>).SnapshotJson
                )
                |> Async.RunSynchronously
                |> ignore)

        ()
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

    let writeUsersAsync (users: (Guid * Events.User * Rvn) list) = asyncResult {
        let users = users |> List.map (fun (guid, user, rvn) -> guid, mapUser user, rvn)

        let writer = persistenceFactory.GetWriter<User, UserEvent> None

        // TODO-MIGRATION: Should not ignore - plus logging?...
        do
            users
            |> List.iter (fun (guid, user, rvn) ->
                writer.CreateFromSnapshotAsync(guid, rvn, (user :> IState<User, UserEvent>).SnapshotJson)
                |> Async.RunSynchronously
                |> ignore)

        ()
    }

    member _.MigrateAsync() = asyncResult {
        let mapUsers (users: (Guid * Event<Events.UserEvent> list * Events.User * Rvn) list) =
            users |> List.map (fun (guid, _, user, rvn) -> guid, user, rvn)

        let sourceUserDic
            (users: (Guid * Event<Events.UserEvent> list * Events.User * Rvn) list)
            : Dictionary<Domain.UserId, string> =
            let dic = Dictionary<Domain.UserId, string>()

            users
            |> List.iter (fun (guid, _, user, _) -> dic.Add(Domain.UserId guid, user.UserName))

            dic

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

        // TODO-MIGRATION: Stop hard-coding partition names...

        let helperFifa2018 =
            PartitionHelper<
                GroupAToH,
                StageFifa,
                Unconfirmed<StageFifa, GroupAToH>,
                PlayerTypeFootball,
                MatchEventFootball,
                Domain.UnconfirmedFifa,
                Domain.MatchEventFootball
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
                Domain.UnconfirmedRwc,
                Domain.MatchEventRugby
             >(
                root,
                "2019-rwc",
                Partition.rwc2019,
                mapFixtureRwc,
                mapSquadRwc,
                persistenceFactory,
                logger
            )

        let helperEuro2021 =
            PartitionHelper<
                GroupAToF,
                StageEuro,
                UnconfirmedEuro,
                PlayerTypeFootball,
                MatchEventFootball,
                Domain.UnconfirmedEuro,
                Domain.MatchEventFootball
             >(
                root,
                "2021-euro",
                Partition.euro2021,
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
                Domain.UnconfirmedFifaV2,
                Domain.MatchEventFootball
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
                Domain.UnconfirmedRwc,
                Domain.MatchEventRugby
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
                Domain.UnconfirmedEuro,
                Domain.MatchEventFootball
             >(
                root,
                "2024-euro",
                Partition.euro2024,
                mapFixtureEuro,
                mapSquadEuro,
                persistenceFactory,
                logger
            )

        let! _ = helperFifa2018.Check()
        let! _ = helperRwc2019.Check()
        let! _ = helperEuro2021.Check()
        let! _ = helperFifa2022.Check()
        let! _ = helperRwc2023.Check()
        let! _ = helperEuro2024.Check()

        let! usersFifa2018 = helperFifa2018.ReadUsers()
        let! usersRwc2019 = helperRwc2019.ReadUsers()
        let! usersEuro2021 = helperEuro2021.ReadUsers()
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

        let! _ = helperFifa2018.MigrateAsync(userMapper.MapperFor(sourceUserDic usersFifa2018))
        let! _ = helperRwc2019.MigrateAsync(userMapper.MapperFor(sourceUserDic usersRwc2019))
        let! _ = helperEuro2021.MigrateAsync(userMapper.MapperFor(sourceUserDic usersEuro2021))
        let! _ = helperFifa2022.MigrateAsync(userMapper.MapperFor(sourceUserDic usersFifa2022))
        let! _ = helperRwc2023.MigrateAsync(userMapper.MapperFor(sourceUserDic usersRwc2023))
        let! _ = helperEuro2024.MigrateAsync(userMapper.MapperFor(sourceUserDic usersEuro2024))

        let! _ = writeUsersAsync (userMapper.Users())

        ()
    }
