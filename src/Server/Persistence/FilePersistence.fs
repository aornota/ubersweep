namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common

open FsToolkit.ErrorHandling
open Microsoft.Extensions.Configuration
open Serilog
open System
open System.Collections.Concurrent
open System.IO
open Thoth.Json.Net

(* TODO-PERSISTENCE:
    -- Write tests for PersistenceModule functions: getEventsFileName | getSnapshotFileName | tryDecodeEventsFileAsync | tryDecodeSnapshotFileAsync | getDirStatusAsync...
    -- Write tests for FileReaderAndWriter (via FilePersistenceFactory): ReadAllAsync | CreateFromSnapshotAsync | WriteEventAsync... *)

// Note that EventsFile, EventLine, Snapshot[File|Line], DirStatus, and FilePersistence module are not private in order to facilitate unit testing.

type EventsFile = {
    EventsFileName: string
    FirstRvn: Rvn
    LastRvn: Rvn
}

type EventLine = EventLine of rvn: Rvn * timestampUtc: DateTime * source: Source * json: Json

type SnapshotFile = { SnapshotFileName: string; Rvn: Rvn }

type SnapshotLine = SnapshotLine of rvn: Rvn * json: Json

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

    let private eventLineDecoder =
        Decode.Auto.generateDecoderCached<EventLine> (Json.caseStrategy, Json.extraCoders)

    let private snapshotLineDecoder =
        Decode.Auto.generateDecoderCached<SnapshotLine> (Json.caseStrategy, Json.extraCoders)

    let private decodeEventLine (Json json) = Decode.fromString eventLineDecoder json

    let private decodeSnapshotLine (Json json) =
        Decode.fromString snapshotLineDecoder json

    let getEventsFileName (Rvn firstRvn, Rvn lastRvn) =
        match firstRvn, lastRvn with
        | 0u, _ -> Error $"First {nameof Rvn} for name of events file must not be {Rvn 0u}"
        | _, 0u -> Error $"Last {nameof Rvn} for name of events file must not be {Rvn 0u}"
        | fromRvn, toRvn when fromRvn > toRvn ->
            Error
                $"First {nameof Rvn} ({Rvn firstRvn}) must not be greater than last {nameof Rvn} ({Rvn lastRvn}) for name of events file"
        | _ -> Ok $"{firstRvn}-{lastRvn}.{eventsFileExtension}"

    let getSnapshotFileName (Rvn rvn) =
        match rvn with
        | 0u -> Error $"{nameof Rvn} for name of snapshot file must not be {Rvn 0u}"
        | _ -> Ok $"{rvn}.{snapshotFileExtension}"

    let tryDecodeEventsFileAsync (dir: DirectoryInfo, guid: Guid) (eventsFile: EventsFile) = asyncResult {
        let rec checkEventLines expectedFirstRvn expectedLastRvn previousRvn eventLines =
            match eventLines, previousRvn with
            | EventLine(rvn, _, _, _) :: t, None ->
                if rvn = expectedFirstRvn then
                    checkEventLines expectedFirstRvn expectedLastRvn (Some rvn) t
                else
                    Error
                        $"Expected first {expectedFirstRvn} for events file {eventsFile.EventsFileName} but decoded first {nameof EventLine} is {rvn}"
            | EventLine(rvn, _, _, _) :: t, Some previousRvn ->
                if rvn = previousRvn.NextRvn then
                    checkEventLines expectedFirstRvn expectedLastRvn (Some rvn) t
                else
                    Error
                        $"Events file {eventsFile.EventsFileName} has decoded {nameof EventLine} with {rvn} not contiguous with previous decoded {nameof EventLine} ({previousRvn})"
            | [], None -> Error $"Events file {eventsFile.EventsFileName} has no decoded {nameof EventLine}s" // should never happen as logic in calling code below ensures that eventLines is not empty on the first call
            | [], Some previousRvn ->
                if previousRvn = expectedLastRvn then
                    Ok()
                else
                    Error
                        $"Expected last {expectedLastRvn} for events file {eventsFile.EventsFileName} but decoded last {nameof EventLine} is {previousRvn}"

        try
            let path = Path.Combine(dir.FullName, guid.ToString())
            let! lines = File.ReadAllLinesAsync(Path.Combine(path, eventsFile.EventsFileName))

            match lines |> List.ofArray with
            | [] -> return! Error $"Events file {eventsFile.EventsFileName} is empty"
            | lines ->
                match
                    lines
                    |> List.map (fun line -> decodeEventLine (Json line))
                    |> List.sequenceResultA
                with
                | Ok eventLines ->
                    let! _ = // error if event lines are invalid
                        checkEventLines eventsFile.FirstRvn eventsFile.LastRvn None eventLines

                    return!
                        Ok(
                            eventLines
                            |> List.map (fun (EventLine(rvn, timestampUtc, source, json)) ->
                                EventJson(rvn, timestampUtc, source, json))
                        )
                | Error errors ->
                    return! Error $"One or more decoding error for events file {eventsFile.EventsFileName}: {errors}"
        with exn ->
            return! Error $"Exception when decoding events file {eventsFile.EventsFileName}: {exn.Message}"
    }

    let tryDecodeSnapshotFileAsync (dir: DirectoryInfo, guid: Guid) (snapshotFile: SnapshotFile) = asyncResult {
        try
            let path = Path.Combine(dir.FullName, guid.ToString())
            let! lines = File.ReadAllLinesAsync(Path.Combine(path, snapshotFile.SnapshotFileName))

            match lines |> List.ofArray with
            | [] -> return! Error $"Snapshot file {snapshotFile.SnapshotFileName} is empty"
            | [ line ] ->
                match decodeSnapshotLine (Json line) with
                | Ok(SnapshotLine(rvn, json)) when rvn = snapshotFile.Rvn -> return! Ok(SnapshotJson(rvn, json))
                | Ok(SnapshotLine(rvn, _)) ->
                    return!
                        Error
                            $"Expected {snapshotFile.Rvn} for snapshot file {snapshotFile.SnapshotFileName} but decoded {nameof SnapshotLine} is {rvn}"
                | Error error ->
                    return! Error $"Decoding error for snapshot file {snapshotFile.SnapshotFileName}: {error}"
            | _ -> return! Error $"Snapshot file {snapshotFile.SnapshotFileName} contains multiople lines"
        with exn ->
            return! Error $"Exception when decoding snapshot file {snapshotFile.SnapshotFileName}: {exn.Message}"
    }

    let getDirStatusAsync (dir: DirectoryInfo, guid: Guid, strictMode) = asyncResult {
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
                    FirstRvn = Rvn fromRvn
                    LastRvn = Rvn toRvn
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

        let checkFiles (eventsFiles: EventsFile list) (snapshotFiles: SnapshotFile list) =
            let getEventsFileName (Rvn firstRvn, Rvn lastRvn) =
                $"{firstRvn}-{lastRvn}.{eventsFileExtension}"

            let getSnapshotFileName (Rvn rvn) = $"{rvn}.{snapshotFileExtension}"

            let rec check (previous: Rvn * Rvn * bool) (files: (Rvn * Rvn * bool) list) =
                match files with
                | (firstRvn, lastRvn, isSnapshot) :: t ->
                    let previousFirstRvn, previousLastRvn, previousIsSnapshot = previous

                    if isSnapshot && previousIsSnapshot then
                        Error
                            $"Snaphot file {getSnapshotFileName firstRvn} follows snapshot file {getSnapshotFileName previousFirstRvn} in {pathForError}"
                    else if not isSnapshot && not previousIsSnapshot then
                        Error
                            $"Events file {getEventsFileName (firstRvn, lastRvn)} follows events file {getEventsFileName (previousFirstRvn, previousLastRvn)} in {pathForError}"
                    else if isSnapshot && firstRvn <> previousLastRvn then
                        Error
                            $"{nameof Rvn} ({firstRvn}) for snapshot file {getSnapshotFileName firstRvn} is not the same as last {nameof Rvn} ({previousLastRvn}) for previous events file {getEventsFileName (previousFirstRvn, previousLastRvn)} in {pathForError}"
                    else if not isSnapshot && firstRvn <> previousLastRvn.NextRvn then
                        Error
                            $"First {nameof Rvn} ({firstRvn}) for events file {getEventsFileName (firstRvn, lastRvn)} is not contiguous with {nameof Rvn} ({previousLastRvn}) for previous snapshot file {getSnapshotFileName previousFirstRvn} in {pathForError}"
                    else
                        check (firstRvn, lastRvn, isSnapshot) t
                | [] -> Ok()

            let eventsFiles =
                eventsFiles
                |> List.map (fun eventsFile -> eventsFile.FirstRvn, eventsFile.LastRvn, false)

            let snapshotFiles =
                snapshotFiles
                |> List.map (fun snapshotFile -> snapshotFile.Rvn, snapshotFile.Rvn, true)

            let allFiles = eventsFiles @ snapshotFiles |> List.sort

            match allFiles with
            | (firstRvn, lastRvn, false) :: _ when firstRvn <> Rvn.InitialRvn ->
                Error
                    $"First {nameof Rvn} ({firstRvn}) is not {Rvn.InitialRvn} for first events file {getEventsFileName (firstRvn, lastRvn)} (and no previous snapshot file) in {pathForError}"
            | (firstRvn, lastRvn, isSnapshot) :: t -> check (firstRvn, lastRvn, isSnapshot) t
            | [] -> Error $"No events files or snapshot files in {pathForError}" // should never happen as logic in calling code below ensures that eventFiles and snapshotFiles cannot both be empty

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

            let! _ = // error if any files with invalid extensions
                let invalidExtensionFileNames =
                    eventsFileNames
                    |> Set.difference (snapshotFileNames |> Set.difference allFileNames)
                    |> List.ofSeq
                    |> List.sort

                match invalidExtensionFileNames with
                | [] -> Ok()
                | _ -> Error $"There are file/s with invalid extensions in {pathForError}: {invalidExtensionFileNames}"

            let! eventsFiles =
                match
                    eventsFileNames
                    |> List.ofSeq
                    |> List.map parseEventsFileName
                    |> List.sequenceResultA
                with
                | Ok eventsFiles ->
                    Ok(
                        eventsFiles
                        |> List.sortBy (fun eventFile -> eventFile.FirstRvn, eventFile.LastRvn)
                    )
                | Error errors -> Error $"There are events file/s with invalid names in {pathForError}: {errors}"

            let! snapshotFiles =
                match
                    snapshotFileNames
                    |> List.ofSeq
                    |> List.map parseSnapshotFileName
                    |> List.sequenceResultA
                with
                | Ok snapshotFiles -> Ok(snapshotFiles |> List.sortBy _.Rvn)
                | Error errors -> Error $"There are snapshot file/s with invalid names in {pathForError}: {errors}"

            let! decodeEventsFilesResults = async {
                if strictMode then
                    return! eventsFiles |> List.map (tryDecodeEventsFileAsync (dir, guid)) |> Async.Parallel
                else
                    return Array.empty
            }

            let! _ = // error if any errors for events files
                decodeEventsFilesResults
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors ->
                    $"One or more strict mode error for events files in {pathForError}: {errors}")

            let! decodeSnapshotFilesResults = async {
                if strictMode then
                    return!
                        snapshotFiles
                        |> List.map (tryDecodeSnapshotFileAsync (dir, guid))
                        |> Async.Parallel
                else
                    return Array.empty
            }

            let! _ = // error if any errors for snapshot files
                decodeSnapshotFilesResults
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors ->
                    $"One or more strict mode error for snapshot files in {pathForError}: {errors}")

            let! result = result {
                match eventsFiles, snapshotFiles with
                | [], [] -> return! Ok Empty
                | [ eventsFile ], [] when eventsFile.FirstRvn = Rvn.InitialRvn -> return! Ok(EventsOnly eventsFile)
                | [ eventsFile ], [] ->
                    return!
                        Error
                            $"First {nameof Rvn} ({eventsFile.FirstRvn}) is not {Rvn.InitialRvn} for only events file in {pathForError}: {eventsFileNames}"
                | _, [] ->
                    return!
                        Error
                            $"There are multiple events files and no snapshot files in {pathForError}: {eventsFileNames}"
                | [], [ snapshotFile ] -> return! Ok(SnapshotOnly snapshotFile) // note: only snapshot file does not have to be Rvn.InitialRvn
                | [], _ ->
                    return!
                        Error
                            $"There are multiple snapshot files and no events files in {pathForError}: {snapshotFileNames}"
                | _ ->
                    let! _ = // error if any errors for [events|snapshot] files
                        checkFiles eventsFiles snapshotFiles

                    let lastSnapshotFile = (snapshotFiles |> List.rev).Head // safe to use Head as empty list will always have been matched above

                    match
                        eventsFiles
                        |> List.tryFind (fun eventsFile -> eventsFile.FirstRvn = lastSnapshotFile.Rvn.NextRvn)
                    with
                    | Some subsequentEventsFile -> return! Ok(SnapshotAndEvents(lastSnapshotFile, subsequentEventsFile))
                    | None -> return! Ok(SnapshotOnly lastSnapshotFile)
            }

            return result
        else
            return! Ok DoesNotExist
    }

type private Input =
    | ReadAll of reply: AsyncReplyChannel<Result<(Guid * NonEmptyList<Entry>) list, string>>
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
        strictMode: bool,
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

    let tryReadAsync (guid: Guid) = asyncResult {
        try
            let! dirStatus = async {
                let! result = FilePersistence.getDirStatusAsync (DirectoryInfo path, guid, strictMode) // use strictMode here when reading

                match result with
                | Ok dirStatus -> return Ok dirStatus
                | Error error -> return Error $"Error when reading {guid} for {pathForError}: {error}"
            }

            let! snapshotFile, eventsFile =
                match dirStatus with
                | DoesNotExist -> Error $"Directory does not exist when reading {guid} for {pathForError}"
                | Empty -> Error $"Directory is empty when reading {guid} for {pathForError}"
                | EventsOnly eventsFile -> Ok(None, Some eventsFile)
                | SnapshotOnly snapshotFile -> Ok(Some snapshotFile, None)
                | SnapshotAndEvents(snapshotFile, eventsFile) -> Ok(Some snapshotFile, Some eventsFile)

            let! snapshotEntry = async {
                match snapshotFile with
                | Some snapshotFile ->
                    let! result = FilePersistence.tryDecodeSnapshotFileAsync (DirectoryInfo path, guid) snapshotFile

                    match result with
                    | Ok snapshotEntry -> return Ok(Some snapshotEntry)
                    | Error error -> return Error $"Error when reading {guid} for {pathForError}: {error}"
                | None -> return Ok None
            }

            let! eventEntries = async {
                match eventsFile with
                | Some eventsFile ->
                    let! result = FilePersistence.tryDecodeEventsFileAsync (DirectoryInfo path, guid) eventsFile

                    match result with
                    | Ok eventEntries -> return Ok eventEntries
                    | Error error -> return Error $"Error when reading {guid} for {pathForError}: {error}"
                | None -> return Ok []
            }

            return!
                match snapshotEntry with
                | Some snapshotEntry -> Ok(NonEmptyList.Create<Entry>(snapshotEntry, eventEntries))
                | None -> NonEmptyList.FromList eventEntries // note: NonEmptyList.FromList should never return Error as logic above ensures that eventEntries is not empty
        with exn ->
            return! Error $"Exception when reading {guid} for {pathForError}: {exn.Message}"
    }

    let tryReadAllAsync () = asyncResult {
        try
            if not (Directory.Exists path) then
                Directory.CreateDirectory path |> ignore<DirectoryInfo>

            let! _ = // error if any files exist
                match (DirectoryInfo path).GetFiles() |> List.ofArray |> List.map _.Name with
                | [] -> Ok()
                | fileNames -> Error $"Files exist when reading all for {pathForError}: {fileNames}"

            let dirAndGuids =
                (DirectoryInfo path).EnumerateDirectories()
                |> List.ofSeq
                |> List.map (fun dir ->
                    match Guid.TryParse(dir.Name) with
                    | true, guid -> dir, Some guid
                    | false, _ -> dir, None)

            let! guids =
                match
                    dirAndGuids
                    |> List.choose (fun (dir, guid) -> if guid.IsNone then Some dir.Name else None)
                with
                | [] -> Ok(dirAndGuids |> List.choose snd)
                | dirNames ->
                    Error $"Non-{nameof Guid} directories exist when reading all for {pathForError}: {dirNames}"

            let! results =
                guids
                |> List.sort
                |> List.map (fun guid -> tryReadAsync guid |> AsyncResult.map (fun list -> guid, list))
                |> Async.Parallel

            return!
                results
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when reading all for {pathForError}: {errors}")
        with exn ->
            return! Error $"Exception when reading all for {pathForError}: {exn.Message}"
    }

    let tryCreateFromSnapshotAsync (guid: Guid, rvn: Rvn, snapshotJson: Json) = asyncResult {
        try
            let! dirStatus = async {
                let! result = FilePersistence.getDirStatusAsync (DirectoryInfo path, guid, false) // ignore strictMode here when creating

                match result with
                | Ok dirStatus -> return Ok dirStatus
                | Error error ->
                    return Error $"Error when creating from snapshot for {guid} for {pathForError}: {error}"
            }

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

            let! snapshotFileName = FilePersistence.getSnapshotFileName rvn
            let snapshotFilePath = Path.Combine(pathForGuid, snapshotFileName)
            let (Json snapshotLine) = Json.encode (SnapshotLine(rvn, snapshotJson))
            do! File.WriteAllLinesAsync(snapshotFilePath, [| snapshotLine |])

            return! Ok()
        with exn ->
            return! Error $"Exception when creating from snapshot for {guid} in {pathForError}: {exn.Message}"
    }

    let tryWriteEventAsync (guid: Guid, rvn: Rvn, source: Source, event: IEvent, getSnapshot: GetSnapshot option) = asyncResult {
        try
            let! dirStatus = async {
                let! result = FilePersistence.getDirStatusAsync (DirectoryInfo path, guid, false) // ignore strictMode here when writing

                match result with
                | Ok dirStatus -> return Ok dirStatus
                | Error error -> return Error $"Error when writing event for {guid} for {pathForError}: {error}"
            }

            let! appendToEventsFile = async {
                match dirStatus with
                | DoesNotExist
                | Empty ->
                    if rvn = Rvn.InitialRvn then
                        return Ok None
                    else
                        return
                            Error
                                $"Directory for {guid} is enpty but {rvn} is not {Rvn.InitialRvn} when writing event for {guid} in {pathForError}"
                | EventsOnly eventsFile
                | SnapshotAndEvents(_, eventsFile) ->
                    if rvn = eventsFile.LastRvn.NextRvn then
                        if strictMode then
                            // This will (among other checks) verify that the last EventLine in the file is actually eventsFile.LastRvn.
                            let! result = FilePersistence.tryDecodeEventsFileAsync (DirectoryInfo path, guid) eventsFile

                            match result with
                            | Ok _ -> return Ok(Some eventsFile)
                            | Error error ->
                                return
                                    Error $"Strict mode error when writing event for {guid} for {pathForError}: {error}"
                        else
                            return Ok(Some eventsFile)
                    else
                        return
                            Error
                                $"{rvn} is inconsistent with latest {nameof Rvn} ({eventsFile.LastRvn}) for events file {eventsFile.EventsFileName} when writing event for {guid} in {pathForError}"
                | SnapshotOnly snapshotFile ->
                    if rvn = snapshotFile.Rvn.NextRvn then
                        if strictMode then
                            // This will (among other checks) verify that the only SnapshotLine in the file is actually snapshotFile.Rvn.
                            let! result =
                                FilePersistence.tryDecodeSnapshotFileAsync (DirectoryInfo path, guid) snapshotFile

                            match result with
                            | Ok _ -> return Ok None
                            | Error error ->
                                return
                                    Error $"Strict mode error when writing event for {guid} for {pathForError}: {error}"
                        else
                            return Ok None
                    else
                        return
                            Error
                                $"{rvn} is inconsistent with {nameof Rvn} ({snapshotFile.Rvn}) for latest snapshot file {snapshotFile.SnapshotFileName} when writing event for {guid} in {pathForError}"
            }

            let pathForGuid = Path.Combine(path, guid.ToString())

            if dirStatus = DoesNotExist then
                Directory.CreateDirectory pathForGuid |> ignore<DirectoryInfo>

            let (Json eventLine) =
                Json.encode (EventLine(rvn, clock.GetUtcNow(), source, event.EventJson))

            match appendToEventsFile with
            | Some eventsFile ->
                let eventsFilePath = Path.Combine(pathForGuid, eventsFile.EventsFileName)
                let! newEventsFileName = FilePersistence.getEventsFileName (eventsFile.FirstRvn, rvn)
                let newEventsFilePath = Path.Combine(pathForGuid, newEventsFileName)
                do! File.AppendAllLinesAsync(eventsFilePath, [| eventLine |])
                File.Move(eventsFilePath, newEventsFilePath)
            | None ->
                let! eventsFileName = FilePersistence.getEventsFileName (rvn, rvn)
                let eventsFilePath = Path.Combine(pathForGuid, eventsFileName)
                do! File.WriteAllLinesAsync(eventsFilePath, [| eventLine |])

            match getSnapshot, snapshotFrequency with
            | Some getSnapshot, Some snapshotFrequency when snapshotFrequency > 1u ->
                if rvn.UInt32 % snapshotFrequency = 0u then
                    let! snapshotFileName = FilePersistence.getSnapshotFileName rvn
                    let snapshotFilePath = Path.Combine(pathForGuid, snapshotFileName)
                    let (Json snapshotLine) = Json.encode (SnapshotJson(rvn, getSnapshot ()))
                    do! File.WriteAllLinesAsync(snapshotFilePath, [| snapshotLine |])
                else
                    ()
            | _ -> ()

            return! Ok()
        with exn ->
            return! Error $"Exception when writing event for {guid} in {pathForError}: {exn.Message}"
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
                    | Error error -> logger.Error("...error when reading all: {error}", error)

                    reply.Reply result
                    return! loop ()
                | CreateFromSnapshot(guid, rvn, snapshotJson, reply) ->
                    logger.Verbose("Creating from snapshot for {guid} ({rvn})...", guid, rvn)

                    let! result = tryCreateFromSnapshotAsync (guid, rvn, snapshotJson)

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

                    let! result = tryWriteEventAsync (guid, rvn, source, event, getSnapsot)

                    match result with
                    | Ok _ -> logger.Verbose("...event written for {guid} ({rvn})", guid, rvn)
                    | Error error ->
                        logger.Error("...error when writing event for {guid} ({rvn}): {error}", guid, rvn, error)

                    reply.Reply result
                    return! loop ()
            }

            loop ())

    do // handler for agent exception
        agent.Error.Add(fun exn -> logger.Error("Exception: {message} at {target}", exn.Message, exn.TargetSite))

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
    let relativeRootKey = "RelativeRoot"

    [<Literal>]
    let snapshotFrequencyKey = "SnapshotFrequency"

    [<Literal>]
    let strictModeKey = "StrictMode"

    [<Literal>]
    let defaultRelativeRoot = "persisted"

    let defaultSnapshotFrequency: uint option = None

    let defaultStrictMode = false

    let configuredOrDefault isConfigured =
        if isConfigured then "configured" else "default"

    let logger = SourcedLogger.Create<FilePersistenceFactory> logger

    do logger.Verbose "Reading configuration"

    let root, isConfiguredRoot =
        let pair =
            try
                let root = config[$"{nameof FilePersistenceFactory}:{relativeRootKey}"]

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
            let key = $"{nameof FilePersistenceFactory}:{snapshotFrequencyKey}"
            let snapshotFrequency = config[key]

            if String.IsNullOrWhiteSpace snapshotFrequency then
                defaultSnapshotFrequency, false
            else
                match UInt32.TryParse snapshotFrequency with
                | true, snapshotFrequency when snapshotFrequency > 1u -> Some snapshotFrequency, true
                | _ ->
                    logger.Warning(
                        "Value {snapshotFrequency} for {key} configuration setting is invalid (must be an integer greater than 1)",
                        snapshotFrequency,
                        key
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

    let strictMode, isConfiguredStrictMode =
        try
            let strictMode = config[$"{nameof FilePersistenceFactory}:{strictModeKey}"]

            if String.IsNullOrWhiteSpace strictMode then
                defaultStrictMode, false
            else
                match bool.TryParse strictMode with
                | true, strictMode -> strictMode, true
                | _ ->
                    logger.Warning(
                        "Value {strictMode} for {migrateOnStartUpKey} configuration setting is invalid (must be an boolean)",
                        strictMode,
                        strictModeKey
                    )

                    defaultStrictMode, false
        with _ ->
            defaultStrictMode, false

    do // logging
        logger.Information(
            "Using {configuredOrDefault} strict mode: {strictMode}",
            configuredOrDefault isConfiguredStrictMode,
            strictMode
        )

    let fileReaderAndWriterDic =
        ConcurrentDictionary<PartitionName option * EntityName, FileReaderAndWriter>()

    let getOrAdd (partitionName, type': Type) =
        let entityName = sanitize type'

        fileReaderAndWriterDic.GetOrAdd(
            (partitionName, entityName),
            (fun _ ->
                new FileReaderAndWriter(root, partitionName, entityName, snapshotFrequency, strictMode, clock, logger))
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
