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
            this.CheckPartitionAsync<GroupAToH, StageFifa, UnconfirmedFifa, PlayerTypeFootball, MatchEventFootball>
                "2018-fifa"

        let! _ =
            this.CheckPartitionAsync<GroupAToD, StageRwc, UnconfirmedRwc, PlayerTypeRugby, MatchEventRugby> "2019-rwc"

        let! _ =
            this.CheckPartitionAsync<GroupAToF, StageEuro, UnconfirmedEuro, PlayerTypeFootball, MatchEventFootball>
                "2021-euro"

        let! _ =
            this.CheckPartitionAsync<GroupAToH, StageFifa, UnconfirmedFifa, PlayerTypeFootball, MatchEventFootball>
                "2022-fifa"

        let! _ =
            this.CheckPartitionAsync<GroupAToD, StageRwc, UnconfirmedRwc, PlayerTypeRugby, MatchEventRugby> "2023-rwc"

        let! _ =
            this.CheckPartitionAsync<GroupAToF, StageEuro, UnconfirmedEuro, PlayerTypeFootball, MatchEventFootball>
                "2024-euro"

        let fifa2018 = Partition.fifa2018 (Path.Combine(root, "2018-fifa"), logger)
        let! fifa2018Users = fifa2018.ReadDrafts()

        let rwc2019 = Partition.rwc2019 (Path.Combine(root, "2019-rwc"), logger)
        let! rwc2019Users = rwc2019.ReadDrafts()

        let euro2021 = Partition.euro2021 (Path.Combine(root, "2021-euro"), logger)
        let! euro2021Users = euro2021.ReadDrafts()

        let fifa2022 = Partition.fifa2022 (Path.Combine(root, "2022-fifa"), logger)
        let! fifa2022Users = fifa2022.ReadDrafts()

        let rwc2023 = Partition.rwc2023 (Path.Combine(root, "2023-rwc"), logger)
        let! rwc2023Users = rwc2023.ReadDrafts()

        let euro2024 = Partition.euro2024 (Path.Combine(root, "2024-euro"), logger)
        let! euro2024Users = euro2024.ReadDrafts()

        (* TODO:
            [-- bail if any Sweepstakes?...]
            -- for each "partition":
                - read events and entities...
                - create User mapping...
                - write Users...
                - write mapped events (or migrated snapshots, e.g. for Users?)...*)

        ()
    }
