namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common

open Microsoft.Extensions.Configuration
open Serilog
open System
open System.Collections.Concurrent

type FilePersistenceFactory(config: IConfiguration, clock: IPersistenceClock, logger: ILogger) =
    [<Literal>]
    let rootKey = "FilePersistence:Root"

    [<Literal>]
    let snapshotFrequencyKey = "FilePersistence:SnapshotFrequency"

    [<Literal>]
    let defaultRoot = "persisted"

    let defaultSnapshotFrequency: uint option = None

    let configuredOrDefault isConfigured =
        if isConfigured then "configured" else "default"

    let logger = SourcedLogger.Create<FilePersistenceFactory> logger

    do logger.Verbose "Reading configuration"

    let root, isConfiguredRoot =
        let pair =
            try
                let root = config[rootKey]

                if String.IsNullOrWhiteSpace root then
                    defaultRoot, false
                else
                    root, true
            with _ ->
                defaultRoot, false

        $@".\{fst pair}", snd pair

    do
        let template =
            $"Using {configuredOrDefault isConfiguredRoot} persistence root: {{root}}"

        logger.Information(template, root)

    let snapshotFrequency, isConfiguredSnapshotFrequency =
        try
            let snapshotFrequency = config[snapshotFrequencyKey]

            if String.IsNullOrWhiteSpace snapshotFrequency then
                defaultSnapshotFrequency, false
            else
                match UInt32.TryParse snapshotFrequency with
                | true, snapshotFrequency when snapshotFrequency > 1u -> Some snapshotFrequency, true
                | _ ->
                    logger.Warning(
                        "Value {snapshotFrequency} for {snapshotFrequencyKey} configuration setting is invalid (must be an integer greater than 1)",
                        snapshotFrequency,
                        snapshotFrequencyKey
                    )

                    defaultSnapshotFrequency, false
        with _ ->
            defaultSnapshotFrequency, false

    do
        let description =
            match snapshotFrequency with
            | Some snapshotFrequency -> $"every {int snapshotFrequency} revisions"
            | None -> "no snapshotting"

        let template =
            $"Using {configuredOrDefault isConfiguredSnapshotFrequency} snapshot frequency: {{description}}"

        logger.Information(template, description)

    let dic =
        ConcurrentDictionary<PartitionName option * EntityName, IReader * IWriter>()

    let getOrAdd (partitionName, entityName) =
        dic.GetOrAdd(
            (partitionName, entityName),
            (fun _ ->
                let readerAndWriter =
                    new FileReaderAndWriter(root, partitionName, entityName, snapshotFrequency, clock, logger)

                readerAndWriter :> IReader, readerAndWriter :> IWriter)
        )

    interface IPersistenceFactory with
        member _.GetReader<'a> partitionName =
            fst (getOrAdd (partitionName, typeof<'a>.Name))

        member _.GetWriter<'a> partitionName =
            snd (getOrAdd (partitionName, typeof<'a>.Name))
