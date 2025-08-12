namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain.Entities

open FsToolkit.ErrorHandling
open System
open System.IO

type private Input =
    | Read of guid: Guid * reply: AsyncReplyChannel<Result<NonEmptyList<Entry>, string>>
    | ReadAll of reply: AsyncReplyChannel<Result<Guid * NonEmptyList<Entry>, string> list>
    | Write of
        guid: Guid *
        rvn: Rvn *
        auditUserId: EntityId<User> *
        eventJson: Json *
        getSnapsot: GetSnapshot *
        reply: AsyncReplyChannel<Result<unit, string>>

// TODO: Add ILogger...
type FileReaderAndWriter
    (
        root: string,
        partitionKey: PartitionKey option,
        entityKey: EntityKey,
        snapshotFrequency: uint option,
        clock: IPersistenceClock
    ) =
    [<Literal>]
    static let fileExtension = "entries"

    let subPath =
        match partitionKey with
        | Some partitionKey -> Path.Combine(partitionKey, entityKey)
        | None -> entityKey

    let path = Path.Combine(root, subPath)

    let pathForError = $"...\{DirectoryInfo(root).Name}\{subPath}"

    let tryRead (guid: Guid) = asyncResult {
        let rec checkConsistency entries lastRvn lastWasSnapshot =
            match entries with
            | h :: t ->
                match h with
                | EventJson(rvn, _, _, _) ->
                    if rvn.IsValidNextRvn lastRvn then
                        checkConsistency t (Some rvn) (Some false)
                    else
                        Error $"{nameof EventJson} with {rvn} inconsistent with previous {nameof Entry} ({lastRvn})"
                | SnapshotJson(rvn, _) ->
                    match lastWasSnapshot, lastRvn with
                    | Some true, _ ->
                        Error
                            $"{nameof SnapshotJson} with {rvn} but previous {nameof Entry} was also {nameof SnapshotJson}"
                    | _, Some lastRvn ->
                        if rvn = lastRvn then
                            checkConsistency t (Some rvn) (Some true)
                        else
                            Error $"{nameof SnapshotJson} with {rvn} not equal to previous {nameof Entry} ({lastRvn})"
                    | _, None -> Error $"{nameof SnapshotJson} with {rvn} but no previous {nameof Entry}"
            | [] -> Ok()

        let rec fromLastSnapshot entries acc =
            match entries with
            | h :: t ->
                match h with
                | EventJson _ -> fromLastSnapshot t (h :: acc)
                | SnapshotJson _ -> h :: acc
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
                        lines |> List.map (fun line -> Json.fromJson<Entry> (Json line))

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
                                $"At least one entry caused a deserialization error when reading {guid} for {pathForError} (e.g. {h})"
                    | _ ->
                        let entries =
                            deserializationResults
                            |> List.choose (fun result ->
                                match result with
                                | Ok entry -> Some entry
                                | Error _ -> None)

                        match checkConsistency entries None None with
                        | Ok _ ->
                            // If there are any snapshots, only return the last snapshot and subsequent entries (if any)
                            let entries = fromLastSnapshot (entries |> List.rev) []
                            return! NonEmptyList<Entry>.FromList entries
                        | Error error ->
                            return! Error $"Consistency check failed when reading {guid} for {pathForError}: {error}"
        with exn ->
            return! Error $"Unexpected error reading {guid} for {pathForError}: {exn.Message}"
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
                    Directory.CreateDirectory path |> ignore
                    return [||]
                with exn ->
                    return [| Error $"Error creating {pathForError} when reading all: {exn.Message}" |]
        with exn ->
            return [| Error $"Unexpected error reading all for {pathForError}: {exn.Message}" |]
    }

    let tryWrite (guid: Guid, rvn: Rvn, auditUserId: EntityId<User>, eventJson: Json, getSnapshot) = asyncResult {
        try
            let guid = guid.ToString() // intentionally shadow as only need string representation
            let file = FileInfo(Path.Combine(path, $"{guid}.{fileExtension}"))
            let utcNow = clock.GetUtcNow()

            match rvn.IsInitialRvn, File.Exists file.FullName with
            | true, true -> return! Error $"File already exists when writing initial {rvn} for {guid} in {pathForError}"
            | false, false ->
                return! Error $"File does not exist when writing non-initial {rvn} for {guid} in {pathForError}"
            | true, false ->
                let (Json eventJson') = Json.toJson (EventJson(rvn, utcNow, auditUserId, eventJson))
                do! File.WriteAllLinesAsync(file.FullName, [| eventJson' |])
                return! Ok()
            | false, true ->
                // Check that the Rvn of the last entry is consistent with the Rvn being written. (Note that unlike when reading, we do not check the consistency of the Rvns for all existing entries.)
                let! lines = File.ReadAllLinesAsync file.FullName

                match lines |> List.ofArray |> List.rev with
                | lastLine :: _ ->
                    match Json.fromJson<Entry> (Json lastLine) with
                    | Ok entry ->
                        if rvn.IsValidNextRvn(Some entry.Rvn) then
                            let (Json eventJson') = Json.toJson (EventJson(rvn, utcNow, auditUserId, eventJson))
                            let (Rvn rvn') = rvn

                            let lines =
                                match snapshotFrequency with
                                | Some snapshotFrequency when snapshotFrequency > 1u && rvn' % snapshotFrequency = 0u ->
                                    let (Json snapshotJson) = Json.toJson (SnapshotJson(rvn, getSnapshot ()))
                                    [| eventJson'; snapshotJson |]
                                | _ -> [| eventJson' |]

                            do! File.AppendAllLinesAsync(file.FullName, lines)
                            return! Ok()
                        else
                            return!
                                Error
                                    $"Previous {nameof Entry} ({entry.Rvn}) not consistent when writing {rvn} for {guid} in {pathForError}"
                    | Error error ->
                        return!
                            Error
                                $"Deserialization error for last entry when writing {rvn} for {guid} in {pathForError}: {error}"
                | [] ->
                    return!
                        Error $"File exists but is empty when writing non-initial {rvn} for {guid} in {pathForError}"
        with exn ->
            return! Error $"Unexpected error writing {rvn} for {guid} in {pathForError}: {exn.Message}"
    }

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop () = async {
                match! inbox.Receive() with
                | Read(guid, reply) ->
                    let! result = tryRead guid
                    reply.Reply result
                    return! loop ()
                | ReadAll reply ->
                    let! result = tryReadAll ()
                    reply.Reply(result |> List.ofArray)
                    return! loop ()
                | Write(guid, rvn, auditUserId, eventJson, getSnapsot, reply) ->
                    let! result = tryWrite (guid, rvn, auditUserId, eventJson, getSnapsot)
                    reply.Reply result
                    return! loop ()
            }

            loop ())

    // TODO: Log unhandled errors (via agent.Error.Add)...

    static member FileExtension = fileExtension

    interface IReader with
        member _.ReadAsync guid =
            agent.PostAndAsyncReply(fun reply -> Read(guid, reply))

        member _.ReadAllAsync() =
            agent.PostAndAsyncReply(fun reply -> ReadAll reply)

    interface IWriter with
        member _.WriteAsync(guid, rvn, auditUserId, eventJson, getSnapshot) =
            agent.PostAndAsyncReply(fun reply -> Write(guid, rvn, auditUserId, eventJson, getSnapshot, reply))

    interface IDisposable with
        member _.Dispose() = agent.Dispose()
