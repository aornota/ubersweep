namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common

open Microsoft.Extensions.Configuration
open System
open System.Collections.Concurrent

type FilePersistenceFactory(config: IConfiguration, clock: IPersistenceClock, logger) =
    [<Literal>]
    let relativeRootKey = "FilePersistence:RelativeRoot"

    [<Literal>]
    let snapshotFrequencyKey = "FilePersistence:SnapshotFrequency"

    [<Literal>]
    let defaultRelativeRoot = "persisted"

    let defaultSnapshotFrequency: uint option = None

    let configuredOrDefault isConfigured =
        if isConfigured then "configured" else "default"

    let logger = SourcedLogger.Create<FilePersistenceFactory> logger

    do logger.Verbose "Reading configuration"

    let root, isConfiguredRoot =
        let pair =
            try
                let root = config[relativeRootKey]

                if String.IsNullOrWhiteSpace root then
                    defaultRelativeRoot, false
                else
                    root, true
            with _ ->
                defaultRelativeRoot, false

        $@".\{fst pair}", snd pair

    do // logging
        logger.Information(
            "Using {configuredOrDefault} persistence root: {root}",
            configuredOrDefault isConfiguredRoot,
            root
        )

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

    do // logging
        let description =
            match snapshotFrequency with
            | Some snapshotFrequency -> $"every {int snapshotFrequency} revisions"
            | None -> "no snapshotting"

        logger.Information(
            "Using {configuredOrDefault} snapshot frequency: {description}",
            configuredOrDefault isConfiguredSnapshotFrequency,
            description
        )

    let fileReaderAndWriterDic =
        ConcurrentDictionary<PartitionName option * EntityName, FileReaderAndWriterLegacy>()

    let getOrAdd (partitionName, type': Type) =
        let entityName = sanitize type'

        fileReaderAndWriterDic.GetOrAdd(
            (partitionName, entityName),
            (fun _ -> new FileReaderAndWriterLegacy(root, partitionName, entityName, snapshotFrequency, clock, logger))
        )

    interface IPersistenceFactory with
        member _.GetReader<'state, 'event when 'state :> IState<'state, 'event>> partitionName =
            getOrAdd (partitionName, typeof<'state>) :> IReader

        member _.GetWriter<'state, 'event when 'state :> IState<'state, 'event>> partitionName =
            getOrAdd (partitionName, typeof<'state>) :> IWriter

    interface IDisposable with
        member _.Dispose() =
            fileReaderAndWriterDic.Values
            |> Seq.iter (fun fileReaderAndWriter -> (fileReaderAndWriter :> IDisposable).Dispose())

            fileReaderAndWriterDic.Clear()
            logger.Warning "Disposed"
