namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open Microsoft.Extensions.Configuration
open Serilog
open System
open System.Collections.Concurrent
open System.IO

type private Input =
    | ReadAll of reply: AsyncReplyChannel<Result<(Guid * NonEmptyList<Entry>) list, string list>>
    | CreateFromSnapshot of guid: Guid * rvn: Rvn * snapshotJson: Json * reply: AsyncReplyChannel<Result<unit, string>>
    | WriteEvent of
        guid: Guid *
        rvn: Rvn *
        source: Source *
        event: IEvent *
        getSnapsot: GetSnapshot option *
        reply: AsyncReplyChannel<Result<unit, string>>

type private FileReaderAndWriter
    (
        root: string,
        partitionName: PartitionName option,
        entityName: EntityName,
        snapshotFrequency: uint option,
        clock: IPersistenceClock,
        logger: ILogger
    ) =
    [<Literal>]
    static let eventsFileExtension = "events"

    [<Literal>]
    static let snapshotFileExtension = "snapshot"

    let subPath =
        match partitionName with
        | Some partitionName -> Path.Combine(partitionName, entityName)
        | None -> entityName

    let logger = SourcedLogger.Create<FileReaderAndWriter>(subPath, logger)

    let path = Path.Combine(root, subPath)

    do // logging
        logger.Information("Using path: {path}", DirectoryInfo(path).FullName)

        let description =
            match snapshotFrequency with
            | Some snapshotFrequency -> $"every {int snapshotFrequency} revisions"
            | None -> "no snapshotting"

        logger.Verbose("Using snapshot frequency: {description}", description)

    let pathForError = $@"...\{DirectoryInfo(root).Name}\{subPath}"

    let tryRead (guid: Guid) : Async<Result<NonEmptyList<Entry'>, string>> = asyncResult {
        // TODO...
        return! Error "TEMP"
    }

    let tryReadAllAsync () : Async<Result<(Guid * NonEmptyList<Entry>) list, string list>> = asyncResult {
        // TODO...
        return! Ok []
    }

    let tryCreateFromSnapshot (guid: Guid, rvn: Rvn, snapshotJson: Json) = asyncResult {
        // TODO...
        return! Ok()
    }

    let tryWriteEvent (guid: Guid, rvn: Rvn, source: Source, event: IEvent, getSnapshot: GetSnapshot option) = asyncResult {
        // TODO...
        return! Ok()
    }

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop () = async {
                match! inbox.Receive() with
                | ReadAll reply ->
                    logger.Verbose("{input}...", nameof ReadAll)

                    let! result = tryReadAllAsync ()

                    match result with
                    | Ok list -> logger.Verbose("...{length} read", list.Length)
                    | Error errors -> logger.Error("...error/s reading all: {errors}", errors)

                    reply.Reply result
                    return! loop ()
                | CreateFromSnapshot(guid, rvn, snapshotJson, reply) ->
                    logger.Verbose(
                        "{input} ({guid} | {rvn} | {snapshotJson})...",
                        nameof CreateFromSnapshot,
                        guid,
                        rvn,
                        snapshotJson
                    )

                    let! result = tryCreateFromSnapshot (guid, rvn, snapshotJson)

                    match result with
                    | Ok _ -> logger.Verbose("...created from snapshot for {guid} ({rvn})", guid, rvn)
                    | Error error ->
                        logger.Error("...error creating from snapshot for {guid} ({rvn}): {error}", guid, rvn, error)

                    reply.Reply result
                    return! loop ()
                | WriteEvent(guid, rvn, source, event, getSnapsot, reply) ->
                    logger.Verbose(
                        "{input} ({guid} | {rvn} | {event} | {source})...",
                        nameof WriteEvent,
                        guid,
                        rvn,
                        event,
                        source
                    )

                    let! result = tryWriteEvent (guid, rvn, source, event, getSnapsot)

                    match result with
                    | Ok _ -> logger.Verbose("...event written for {guid} ({rvn})", guid, rvn)
                    | Error error ->
                        logger.Error("...error writing event for {guid} ({rvn}): {error}", guid, rvn, error)

                    reply.Reply result
                    return! loop ()
            }

            loop ())

    do agent.Error.Add(fun exn -> logger.Error("Unexpected error: {message} at {target}", exn.Message, exn.TargetSite))

    static member EventsFileExtension = eventsFileExtension
    static member SnapshotFileExtension = snapshotFileExtension

    interface IReader with
        member _.ReadAllAsync() =
            agent.PostAndAsyncReply(fun reply -> ReadAll reply)

    interface IWriter with
        member _.CreateFromSnapshotAsync(guid, rvn, snapshotJson) =
            agent.PostAndAsyncReply(fun reply -> CreateFromSnapshot(guid, rvn, snapshotJson, reply))

        member _.WriteEventAsync(guid, rvn, source, event, getSnapshot) =
            agent.PostAndAsyncReply(fun reply -> WriteEvent(guid, rvn, source, event, getSnapshot, reply))

    interface IDisposable with
        member _.Dispose() =
            agent.Dispose()
            logger.Warning "Disposed"

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
        ConcurrentDictionary<PartitionName option * EntityName, FileReaderAndWriter>()

    let getOrAdd (partitionName, type': Type) =
        let entityName = sanitize type'

        fileReaderAndWriterDic.GetOrAdd(
            (partitionName, entityName),
            (fun _ -> new FileReaderAndWriter(root, partitionName, entityName, snapshotFrequency, clock, logger))
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
