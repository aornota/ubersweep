namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common

open FsToolkit.ErrorHandling
open Microsoft.Extensions.Configuration
open Serilog
open System
open System.Collections.Concurrent
open System.IO

// Note that EventsFile | SnapshotFile | DirStatus (and FilePersistence module) are not private to faciliate unit testing.

type EventsFile = {
    EventsFileName: string
    FromRvn: Rvn
    ToRvn: Rvn
}

type SnapshotFile = { SnapshotFileName: string; Rvn: Rvn }

type DirStatus =
    | DoesNotExist
    | Empty
    | EventsOnly of eventsFile: EventsFile
    | SnapshotOnly of snapshotFile: SnapshotFile
    | SnapshotAndEvents of snapshotFile: SnapshotFile * eventsFile: EventsFile

[<RequireQualifiedAccess>]
module FilePersistence =
    [<Literal>]
    let eventsFileExtension = "events"

    [<Literal>]
    let snapshotFileExtension = "snapshot"

    let getEventsFileName (Rvn fromRvn, Rvn toRvn) =
        $"{fromRvn}-{toRvn}.{eventsFileExtension}"

    let getSnapshotFileName (Rvn rvn) = $"{rvn}.{snapshotFileExtension}"

    let getDirStatus (dir: DirectoryInfo, guid: Guid) : Result<DirStatus, string> = result {
        let parseEventsFileName (fileName: string) =
            let fileNameWithoutExtension = Path.GetFileNameWithoutExtension fileName

            let parsed =
                match fileNameWithoutExtension.Split [| '-' |] |> List.ofArray with
                | [ fromRvn; toRvn ] ->
                    match UInt32.TryParse fromRvn, UInt32.TryParse toRvn with
                    | (true, 0u), _
                    | (false, _), _
                    | _, (true, 0u)
                    | _, (false, _) -> None
                    | (true, fromRvn), (true, toRvn) -> Some(fromRvn, toRvn)
                | _ -> None

            match parsed with
            | Some(fromRvn, toRvn) when fromRvn > toRvn ->
                Error
                    $"First revision ({fromRvn}) is greater than last revision ({toRvn}) in name for events file {fileName}"
            | Some(fromRvn, toRvn) ->
                Ok {
                    EventsFileName = fileName
                    FromRvn = Rvn fromRvn
                    ToRvn = Rvn toRvn
                }
            | None -> Error $"Invalid name for events file {fileName}"

        let parseSnapshotFileName (fileName: string) =
            match UInt32.TryParse(Path.GetFileNameWithoutExtension fileName) with
            | true, 0u
            | false, _ -> Error $"Invalid name for snapshot file {fileName}"
            | true, rvn ->
                Ok {
                    SnapshotFileName = fileName
                    Rvn = Rvn rvn
                }

        let path = Path.Combine(dir.FullName, guid.ToString())
        let pathForError = $@"...\{dir.Name}\{guid}"

        if Directory.Exists path then
            let allFileNames =
                (DirectoryInfo path).GetFiles() |> Array.map _.Name |> Set.ofArray

            let eventsFileNames =
                (DirectoryInfo path).GetFiles $"*.{eventsFileExtension}"
                |> Array.map _.Name
                |> Set.ofArray

            let snapshotFileNames =
                (DirectoryInfo path).GetFiles $"*.{snapshotFileExtension}"
                |> Array.map _.Name
                |> Set.ofArray

            let! _ =
                let invalidExtensionFileNames =
                    eventsFileNames
                    |> Set.difference (snapshotFileNames |> Set.difference allFileNames)
                    |> List.ofSeq
                    |> List.sortBy id

                match invalidExtensionFileNames with
                | [] -> Ok()
                | _ -> Error $"There are file/s with invalid extensions in {pathForError}: {invalidExtensionFileNames}"

            let! eventsFiles =
                match
                    eventsFileNames
                    |> List.ofSeq
                    |> List.sortBy id
                    |> List.map parseEventsFileName
                    |> List.sequenceResultA
                with
                | Ok eventsFiles -> Ok eventsFiles
                | Error errors -> Error $"There are events file/s with invalid names in {pathForError}: {errors}"

            let! snapshotFiles =
                match
                    snapshotFileNames
                    |> List.ofSeq
                    |> List.sortBy id
                    |> List.map parseSnapshotFileName
                    |> List.sequenceResultA
                with
                | Ok snapshotFiles -> Ok snapshotFiles
                | Error errors -> Error $"There are snapshot file/s with invalid names in {pathForError}: {errors}"

            return!
                match eventsFiles, snapshotFiles with
                | [], [] -> Ok Empty
                | [ eventsFile ], [] when eventsFile.FromRvn = Rvn.InitialRvn -> Ok(EventsOnly eventsFile)
                | [ eventsFile ], [] ->
                    Error
                        $"First {nameof Rvn} ({eventsFile.FromRvn}) is not {Rvn.InitialRvn} for only events file in {pathForError}: {eventsFileNames}"
                | _, [] ->
                    Error $"There are multiple events files and no snapshot files in {pathForError}: {eventsFileNames}"
                | [], [ snapshotFile ] -> Ok(SnapshotOnly snapshotFile) // does not have to be Rvn.InitialRvn
                | [], _ ->
                    Error
                        $"There are multiple snapshot files and no events files in {pathForError}: {snapshotFileNames}"
                | _ ->
                    (* TODO-PERSISTENCE: Check consistency of events and snapshot files:
                        -- First events file must be from Rvn.InitialRvn (unless follows snapshot file)...
                        -- Events and snapshot files must be contiguous... *)

                    let lastSnapshotFile = (snapshotFiles |> List.rev).Head // safe to use Head as empty list will have been matched above

                    match
                        eventsFiles
                        |> List.tryFind (fun eventsFile -> eventsFile.FromRvn = lastSnapshotFile.Rvn.NextRvn)
                    with
                    | Some subsequentEventsFile -> Ok(SnapshotAndEvents(lastSnapshotFile, subsequentEventsFile))
                    | None -> Ok(SnapshotOnly lastSnapshotFile)
        else
            return! Ok DoesNotExist
    }

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

    let tryRead (guid: Guid) : Async<Result<NonEmptyList<Entry'>, string>> = asyncResult { // TODO...
        return! Error "TEMP"
    }

    let tryReadAllAsync () : Async<Result<(Guid * NonEmptyList<Entry>) list, string list>> = asyncResult { // TODO...
        return! Ok []
    }

    let tryCreateFromSnapshot (guid: Guid, rvn: Rvn, snapshotJson: Json) = asyncResult {
        try
            let! dirStatus = FilePersistence.getDirStatus (DirectoryInfo path, guid)

            let! dirExists =
                match dirStatus with
                | DoesNotExist -> Ok false
                | Empty -> Ok true
                | EventsOnly _
                | SnapshotOnly _
                | SnapshotAndEvents _ ->
                    Error $"Directory for {guid} is not empty when creating from snapshot in {pathForError}"

            let pathForGuid = Path.Combine(path, guid.ToString())

            if not dirExists then
                Directory.CreateDirectory pathForGuid |> ignore<DirectoryInfo>

            let snapshotFilePath =
                Path.Combine(pathForGuid, FilePersistence.getSnapshotFileName rvn)

            let (Json snapshotLine) = Json.toJson (SnapshotJson(rvn, snapshotJson))

            do! File.WriteAllLinesAsync(snapshotFilePath, [| snapshotLine |])

            return! Ok()
        with exn ->
            return! Error $"Error when creating from snapshot for {guid} in {pathForError}: {exn.Message}"
    }

    let tryWriteEvent (guid: Guid, rvn: Rvn, source: Source, event: IEvent, getSnapshot: GetSnapshot option) = asyncResult {
        try
            let! dirStatus = FilePersistence.getDirStatus (DirectoryInfo path, guid)

            let! appendToEventsFile =
                match dirStatus with
                | DoesNotExist
                | Empty ->
                    if rvn = Rvn.InitialRvn then
                        Ok None
                    else
                        Error
                            $"Directory for {guid} is enpty but {rvn} is not {Rvn.InitialRvn} when writing event for {guid} in {pathForError}"
                | EventsOnly eventsFile
                | SnapshotAndEvents(_, eventsFile) ->
                    if rvn = eventsFile.ToRvn.NextRvn then
                        // TODO-PERSISTENCE: Read eventsFile and check that last line is actually ToRvn? Only for "strict mode"?...
                        Ok(Some eventsFile)
                    else
                        Error
                            $"{rvn} is inconsistent with latest {nameof Rvn} ({eventsFile.ToRvn}) for events file {eventsFile.EventsFileName} when writing event for {guid} in {pathForError}"
                | SnapshotOnly snapshotFile ->
                    if rvn = snapshotFile.Rvn.NextRvn then
                        // TODO-PERSISTENCE: Read snapshotFile and check that only line is actually Rvn? Only for "strict mode"?...
                        Ok None
                    else
                        Error
                            $"{rvn} is inconsistent with {nameof Rvn} ({snapshotFile.Rvn}) for latest snapshot file {snapshotFile.SnapshotFileName} when writing event for {guid} in {pathForError}"

            let pathForGuid = Path.Combine(path, guid.ToString())

            if dirStatus = DoesNotExist then
                Directory.CreateDirectory pathForGuid |> ignore<DirectoryInfo>

            let (Json eventLine) =
                Json.toJson (EventJson(rvn, clock.GetUtcNow(), source, event.EventJson))

            match appendToEventsFile with
            | Some eventsFile ->
                let eventsFilePath = Path.Combine(pathForGuid, eventsFile.EventsFileName)

                let newEventsFilePath =
                    Path.Combine(pathForGuid, FilePersistence.getEventsFileName (eventsFile.FromRvn, rvn))

                do! File.AppendAllLinesAsync(eventsFilePath, [| eventLine |])
                File.Move(eventsFilePath, newEventsFilePath)
            | None ->
                let eventsFilePath =
                    Path.Combine(pathForGuid, FilePersistence.getEventsFileName (rvn, rvn))

                do! File.WriteAllLinesAsync(eventsFilePath, [| eventLine |])

            match getSnapshot, snapshotFrequency with
            | Some getSnapshot, Some snapshotFrequency when snapshotFrequency > 1u ->
                let (Rvn rvn') = rvn

                if rvn' % snapshotFrequency = 0u then
                    let snapshotFilePath =
                        Path.Combine(pathForGuid, FilePersistence.getSnapshotFileName rvn)

                    let (Json snapshotLine) = Json.toJson (SnapshotJson(rvn, getSnapshot ()))
                    do! File.WriteAllLinesAsync(snapshotFilePath, [| snapshotLine |])
                else
                    ()
            | _ -> ()

            return! Ok()
        with exn ->
            return! Error $"Error when writing event for {guid} in {pathForError}: {exn.Message}"
    }

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop () = async {
                match! inbox.Receive() with
                | ReadAll reply ->
                    logger.Verbose "Reading all..."

                    let! result = tryReadAllAsync ()

                    match result with
                    | Ok list -> logger.Verbose("...all ({length}) read", list.Length)
                    | Error errors -> logger.Error("...error/s when reading all: {errors}", errors)

                    reply.Reply result
                    return! loop ()
                | CreateFromSnapshot(guid, rvn, snapshotJson, reply) ->
                    logger.Verbose("Creating from snapshot for {guid} ({rvn})...", guid, rvn)

                    let! result = tryCreateFromSnapshot (guid, rvn, snapshotJson)

                    match result with
                    | Ok _ -> logger.Verbose("...created from snapshot for {guid} ({rvn})", guid, rvn)
                    | Error error ->
                        logger.Error(
                            "...error when creating from snapshot for {guid} ({rvn}): {error}",
                            guid,
                            rvn,
                            error
                        )

                    reply.Reply result
                    return! loop ()
                | WriteEvent(guid, rvn, source, event, getSnapsot, reply) ->
                    logger.Verbose("Writing event for {guid} ({rvn})...", guid, rvn)

                    let! result = tryWriteEvent (guid, rvn, source, event, getSnapsot)

                    match result with
                    | Ok _ -> logger.Verbose("...event written for {guid} ({rvn})", guid, rvn)
                    | Error error ->
                        logger.Error("...error when writing event for {guid} ({rvn}): {error}", guid, rvn, error)

                    reply.Reply result
                    return! loop ()
            }

            loop ())

    do agent.Error.Add(fun exn -> logger.Error("Unexpected error: {message} at {target}", exn.Message, exn.TargetSite))

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
