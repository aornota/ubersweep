namespace Aornota.Ubersweep.Server.Persistence

// TODO-PERSISTENCE: Remove this (and related tests) once new FileReaderAndWriter has been implemeneted (with tests)...

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open Serilog
open System
open System.IO

type private Input' =
    | Read of guid: Guid * reply: AsyncReplyChannel<Result<NonEmptyList<Entry'>, string>>
    | ReadAll of reply: AsyncReplyChannel<Result<Guid * NonEmptyList<Entry'>, string> list>
    | CreateFromSnapshot of guid: Guid * rvn: Rvn * snapshotJson: Json * reply: AsyncReplyChannel<Result<unit, string>>
    | WriteEvent of
        guid: Guid *
        rvn: Rvn *
        auditUserId: UserId *
        event: IEvent *
        getSnapsot: GetSnapshot option *
        reply: AsyncReplyChannel<Result<unit, string>>

type FileReaderAndWriterLegacy
    (
        root: string,
        partitionName: PartitionName option,
        entityName: EntityName,
        snapshotFrequency: uint option,
        clock: IPersistenceClock,
        logger: ILogger
    ) =
    [<Literal>]
    static let fileExtension = "entries"

    let subPath =
        match partitionName with
        | Some partitionName -> Path.Combine(partitionName, entityName)
        | None -> entityName

    let logger = SourcedLogger.Create<FileReaderAndWriterLegacy>(subPath, logger)

    let path = Path.Combine(root, subPath)

    do // logging
        logger.Information("Using path: {path}", DirectoryInfo(path).FullName)

        let description =
            match snapshotFrequency with
            | Some snapshotFrequency -> $"every {int snapshotFrequency} revisions"
            | None -> "no snapshotting"

        logger.Verbose("Using snapshot frequency: {description}", description)

    let pathForError = $@"...\{DirectoryInfo(root).Name}\{subPath}"

    let tryRead (guid: Guid) = asyncResult {
        let rec checkConsistency entries lastRvn lastWasSnapshot =
            match entries with
            | h :: t ->
                match h with
                | EventJson'(rvn, _, _, _) ->
                    if rvn.IsValidNextRvn lastRvn then
                        checkConsistency t (Some rvn) (Some false)
                    else
                        Error $"{nameof EventJson'} with {rvn} inconsistent with previous {nameof Entry'} ({lastRvn})"
                | SnapshotJson'(rvn, _) ->
                    match lastWasSnapshot, lastRvn with
                    | Some true, _ ->
                        Error
                            $"{nameof SnapshotJson'} with {rvn} but previous {nameof Entry'} was also {nameof SnapshotJson'}"
                    | _, Some lastRvn ->
                        if rvn = lastRvn then
                            checkConsistency t (Some rvn) (Some true)
                        else
                            Error $"{nameof SnapshotJson'} with {rvn} not equal to previous {nameof Entry'} ({lastRvn})"
                    | _, None ->
                        // If the first entry is a snapshot, it does not need to be InitialRvn.
                        checkConsistency t (Some rvn) (Some true)
            | [] -> Ok()

        let rec fromLastSnapshot entries acc =
            match entries with
            | h :: t ->
                match h with
                | EventJson' _ -> fromLastSnapshot t (h :: acc)
                | SnapshotJson' _ -> h :: acc
            | [] -> acc

        try
            let file = FileInfo(Path.Combine(path, $"{guid}.{fileExtension}"))

            if not (File.Exists file.FullName) then
                return! Error $"File does not exist when reading {guid} for {pathForError}"
            else
                let! lines = File.ReadAllLinesAsync file.FullName

                match lines |> List.ofArray with
                | [] -> return! Error $"File exists but is empty when reading {guid} for {pathForError}"
                | lines ->
                    let deserializationResults =
                        lines |> List.map (fun line -> Json.decode<Entry'> (Json line))

                    match
                        deserializationResults
                        |> List.choose (fun result ->
                            match result with
                            | Ok _ -> None
                            | Error error -> Some error)
                    with
                    | h :: _ ->
                        return!
                            Error
                                $"At least one line caused a deserialization error when reading {guid} for {pathForError} (e.g. {h})"
                    | _ ->
                        let entries =
                            deserializationResults
                            |> List.choose (fun result ->
                                match result with
                                | Ok entry -> Some entry
                                | Error _ -> None)

                        match checkConsistency entries None None with
                        | Ok() ->
                            // If there are any snapshots, only return the last snapshot and subsequent entries (if any)
                            let entries = fromLastSnapshot (entries |> List.rev) []
                            return! NonEmptyList<Entry'>.FromList entries
                        | Error error ->
                            return! Error $"Consistency check failed when reading {guid} for {pathForError}: {error}"
        with exn ->
            return! Error $"Unexpected error when reading {guid} for {pathForError}: {exn.Message}"
    }

    let tryReadAll () = async {
        try
            if Directory.Exists path then
                let files =
                    (DirectoryInfo path).GetFiles $"*.{fileExtension}"
                    |> List.ofArray
                    |> List.map (fun file ->
                        match Guid.TryParse(Path.GetFileNameWithoutExtension file.Name) with
                        | true, guid -> file, Some guid
                        | false, _ -> file, None)

                match
                    files
                    |> List.choose (fun (file, guid) -> if guid.IsNone then Some file else None)
                with
                | h :: _ ->
                    return [|
                        Error
                            $"At least one .{fileExtension} file in {pathForError} has a non-{nameof Guid} name (e.g. {h.Name})"
                    |]
                | _ ->
                    return!
                        files
                        |> List.choose snd
                        |> List.sort
                        |> List.map (fun guid -> tryRead guid |> AsyncResult.map (fun list -> guid, list))
                        |> Async.Parallel
            else
                try
                    Directory.CreateDirectory path |> ignore<DirectoryInfo>
                    return [||]
                with exn ->
                    return [| Error $"Error creating {pathForError} when reading all: {exn.Message}" |]
        with exn ->
            return [|
                Error $"Unexpected error when reading all for {pathForError}: {exn.Message}"
            |]
    }

    let tryCreateFromSnapshot (guid: Guid, rvn: Rvn, snapshotJson: Json) = asyncResult {
        try
            let file = FileInfo(Path.Combine(path, $"{guid}.{fileExtension}"))

            match File.Exists file.FullName with
            | true ->
                return! Error $"File already exists when creating from snapshot for {rvn} for {guid} in {pathForError}"
            | false ->
                let (Json snapshotJson') = Json.encode (SnapshotJson'(rvn, snapshotJson))
                do! File.WriteAllLinesAsync(file.FullName, [| snapshotJson' |])

                return! Ok()
        with exn ->
            return!
                Error
                    $"Unexpected error when creating from snapshot for {rvn} for {guid} in {pathForError}: {exn.Message}"
    }

    let tryWriteEvent (guid: Guid, rvn: Rvn, auditUserId: UserId, event: IEvent, getSnapshot) = asyncResult {
        try
            let file = FileInfo(Path.Combine(path, $"{guid}.{fileExtension}"))
            let utcNow = clock.GetUtcNow()

            match rvn.IsInitialRvn, File.Exists file.FullName with
            | true, true ->
                return! Error $"File already exists when writing event for initial {rvn} for {guid} in {pathForError}"
            | false, false ->
                return!
                    Error $"File does not exist when writing event for non-initial {rvn} for {guid} in {pathForError}"
            | true, false ->
                let (Json eventJson) =
                    Json.encode (EventJson'(rvn, utcNow, auditUserId, event.EventJson))

                do! File.WriteAllLinesAsync(file.FullName, [| eventJson |])
                return! Ok()
            | false, true ->
                // Check that the Rvn of the last entry is consistent with the Rvn being written. (Note that unlike when reading, we do not check the consistency of the Rvns for all existing entries.)
                let! lines = File.ReadAllLinesAsync file.FullName

                match lines |> List.ofArray |> List.rev with
                | lastLine :: _ ->
                    match Json.decode<Entry'> (Json lastLine) with
                    | Ok entry ->
                        if rvn.IsValidNextRvn(Some entry.Rvn) then
                            let (Json eventJson) =
                                Json.encode (EventJson'(rvn, utcNow, auditUserId, event.EventJson))

                            let (Rvn rvn') = rvn

                            // Note that we only write a snapshot if configured to do so - and if we have been given a function to get the snapshot.
                            let lines =
                                match getSnapshot, snapshotFrequency with
                                | Some getSnapshot, Some snapshotFrequency when
                                    snapshotFrequency > 1u && rvn' % snapshotFrequency = 0u
                                    ->
                                    let (Json snapshotJson) = Json.encode (SnapshotJson'(rvn, getSnapshot ()))
                                    [| eventJson; snapshotJson |]
                                | _ -> [| eventJson |]

                            do! File.AppendAllLinesAsync(file.FullName, lines)
                            return! Ok()
                        else
                            return!
                                Error
                                    $"Previous {nameof Entry'} ({entry.Rvn}) not consistent when writing event for {rvn} for {guid} in {pathForError}"
                    | Error error ->
                        return!
                            Error
                                $"Deserialization error for last entry when writing event for {rvn} for {guid} in {pathForError}: {error}"
                | [] ->
                    return!
                        Error
                            $"File exists but is empty when writing event for non-initial {rvn} for {guid} in {pathForError}"
        with exn ->
            return! Error $"Unexpected error when writing event for {rvn} for {guid} in {pathForError}: {exn.Message}"
    }

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop () = async {
                match! inbox.Receive() with
                | Read(guid, reply) ->
                    logger.Verbose("{input} ({guid})...", nameof Read, guid)

                    let! result = tryRead guid

                    match result with
                    | Ok nonEmptyList -> logger.Verbose("...{length} read for {guid}", nonEmptyList.List.Length, guid)
                    | Error error -> logger.Error("...error reading {guid}: {error}", guid, error)

                    reply.Reply result
                    return! loop ()
                | ReadAll reply ->
                    logger.Verbose("{input}...", nameof ReadAll)

                    let! result = tryReadAll ()

                    logger.Verbose("...all read ({length} results)", result.Length)

                    reply.Reply(result |> List.ofArray)
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
                    | Ok _ -> logger.Verbose("...snapshot written for {guid} ({rvn})", guid, rvn)
                    | Error error ->
                        logger.Error("...error writing snapshot for {guid} ({rvn}): {error}", guid, rvn, error)

                    reply.Reply result
                    return! loop ()
                | WriteEvent(guid, rvn, auditUserId, event, getSnapsot, reply) ->
                    logger.Verbose(
                        "{input} ({guid} | {rvn} | {event} | {auditUserId})...",
                        nameof WriteEvent,
                        guid,
                        rvn,
                        event,
                        auditUserId
                    )

                    let! result = tryWriteEvent (guid, rvn, auditUserId, event, getSnapsot)

                    match result with
                    | Ok _ -> logger.Verbose("...event written for {guid} ({rvn})", guid, rvn)
                    | Error error ->
                        logger.Error("...error writing event for {guid} ({rvn}): {error}", guid, rvn, error)

                    reply.Reply result
                    return! loop ()
            }

            loop ())

    do agent.Error.Add(fun exn -> logger.Error("Unexpected error: {message} at {target}", exn.Message, exn.TargetSite))

    static member FileExtension = fileExtension

    interface IReader' with
        member _.ReadAsync' guid =
            agent.PostAndAsyncReply(fun reply -> Read(guid, reply))

        member _.ReadAllAsync'() =
            agent.PostAndAsyncReply(fun reply -> ReadAll reply)

    interface IWriter' with
        member _.CreateFromSnapshotAsync'(guid, rvn, snapshotJson) =
            agent.PostAndAsyncReply(fun reply -> CreateFromSnapshot(guid, rvn, snapshotJson, reply))

        member _.WriteEventAsync'(guid, rvn, auditUserId, event, getSnapshot) =
            agent.PostAndAsyncReply(fun reply -> WriteEvent(guid, rvn, auditUserId, event, getSnapshot, reply))

    interface IDisposable with
        member _.Dispose() =
            agent.Dispose()
            logger.Warning "Disposed"
