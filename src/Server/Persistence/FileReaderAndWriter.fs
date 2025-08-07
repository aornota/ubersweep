namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common.JsonConverter
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain

open System
open System.IO

type private Input =
    | Read of guid: Guid * reply: AsyncReplyChannel<Result<Entry list, string>>
    | ReadAll of reply: AsyncReplyChannel<Result<Entry list, string> list>
    | Write of
        guid: Guid *
        rvn: Rvn *
        auditUserId: UserId *
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
    let fileExtension = "entries"

    let path =
        match partitionKey with
        | Some partitionKey -> Path.Combine(root, partitionKey, entityKey)
        | None -> Path.Combine(root, entityKey)

    let tryRead (guid: Guid) =
        let rec checkConsistency entries lastRvn lastWasSnapshot =
            match entries with
            | h :: t ->
                match h with
                | EventJson(rvn, _, _, _) ->
                    if rvn.IsValidNextRvn lastRvn then
                        checkConsistency t (Some rvn) (Some false)
                    else
                        Error $"{nameof EventJson} with {rvn} inconsistent with previous entry ({lastRvn})"
                | SnapshotJson(rvn, _) ->
                    match lastWasSnapshot, lastRvn with
                    | Some true, _ ->
                        Error $"{nameof SnapshotJson} with {rvn} but previous entry was also {nameof SnapshotJson}"
                    | _, Some lastRvn ->
                        if rvn = lastRvn then
                            checkConsistency t (Some rvn) (Some true)
                        else
                            Error $"{nameof SnapshotJson} with {rvn} not equal to previous entry ({lastRvn})"
                    | _, None -> Error $"{nameof SnapshotJson} with {rvn} but no previous entry"
            | [] -> Ok()

        let rec fromLastSnapshot entries acc =
            match entries with
            | h :: t ->
                match h with
                | EventJson _ -> fromLastSnapshot t (h :: acc)
                | SnapshotJson _ -> h :: acc
            | [] -> acc

        try
            let guid = guid.ToString() // intentionally shadow as only need string representation
            let file = FileInfo(Path.Combine(path, $"{guid}.{fileExtension}"))

            if not (File.Exists file.FullName) then
                Error $"File does not exist when reading {guid} for {path}"
            else
                match File.ReadAllLines file.FullName |> List.ofArray with
                | [] -> Error $"File exists but is empty when reading {guid} for {path}"
                | lines ->
                    let deserializationResults =
                        lines |> List.map (fun line -> fromJson<Entry> (Json line))

                    match
                        deserializationResults
                        |> List.choose (fun result ->
                            match result with
                            | Ok _ -> None
                            | Error error -> Some error)
                    with
                    | h :: _ ->
                        Error
                            $"At least one entry caused a deserialization error when reading {guid} for {path} (e.g. {h})"
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
                            Ok(fromLastSnapshot (entries |> List.rev) [])
                        | Error error -> Error $"Consistency check failed when reading {guid} for {path}: {error}"
        with exn ->
            Error $"Unexpected error reading {guid} for {path}: {exn.Message}"

    let tryReadAll () =
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
                | h :: _ -> [
                    Error $"At least one .{fileExtension} file in {path} has a non-{nameof Guid} name (e.g. {h.Name})"
                  ]
                | _ -> files |> List.choose snd |> List.map tryRead
            else
                try
                    Directory.CreateDirectory path |> ignore
                    []
                with exn -> [ Error $"Error creating {path} when reading all: {exn.Message}" ]
        with exn -> [ Error $"Unexpected error reading all for {path}: {exn.Message}" ]

    let tryWrite (guid: Guid, rvn: Rvn, auditUserId: UserId, eventJson: Json, getSnapshot) =
        try
            let guid = guid.ToString() // intentionally shadow as only need string representation
            let file = FileInfo(Path.Combine(path, $"{guid}.{fileExtension}"))
            let utcNow = clock.GetUtcNow()

            match rvn.IsInitialRvn, File.Exists file.FullName with
            | true, true -> Error $"File already exists when writing initial ({rvn}) for {guid} in {path}"
            | false, false -> Error $"File does not exist when writing non-initial ({rvn}) for {guid} in {path}"
            | true, false ->
                let (Json eventJson') = toJson (EventJson(rvn, utcNow, auditUserId, eventJson))
                File.WriteAllLines(file.FullName, [| eventJson' |])
                Ok()
            | false, true ->
                // Check that the Rvn of the last entry is consistent with the Rvn being written. (Note that unlike when reading, we do not check the consistency of the Rvns for all existing entries.)
                match File.ReadAllLines file.FullName |> List.ofArray |> List.rev with
                | lastLine :: _ ->
                    match fromJson<Entry> (Json lastLine) with
                    | Ok entry ->
                        if rvn.IsValidNextRvn(Some entry.Rvn) then
                            let (Json eventJson') = toJson (EventJson(rvn, utcNow, auditUserId, eventJson))
                            let (Rvn rvn') = rvn

                            let lines =
                                match snapshotFrequency with
                                | Some snapshotFrequency when snapshotFrequency > 1u && rvn' % snapshotFrequency = 0u ->
                                    let (Json snapshotJson) = toJson (SnapshotJson(rvn, getSnapshot ()))
                                    [| eventJson'; snapshotJson |]
                                | _ -> [| eventJson' |]

                            File.AppendAllLines(file.FullName, lines)
                            Ok()
                        else
                            Error $"Previous ({entry.Rvn}) not consistent when writing {rvn} for {guid} in {path}"
                    | Error error ->
                        Error $"Deserialization error for last entry when writing {rvn} for {guid} in {path}: {error}"
                | [] -> Error $"File exists but is empty when writing non-initial {rvn} for {guid} in {path}"
        with exn ->
            Error $"Unexpected error writing {rvn} for {guid} in {path}: {exn.Message}"

    let agent =
        MailboxProcessor.Start(fun inbox ->
            let rec loop () = async {
                match! inbox.Receive() with
                | Read(guid, reply) ->
                    reply.Reply(tryRead guid)
                    return! loop ()
                | ReadAll reply ->
                    reply.Reply(tryReadAll ())
                    return! loop ()
                | Write(guid, rvn, auditUserId, eventJson, getSnapsot, reply) ->
                    reply.Reply(tryWrite (guid, rvn, auditUserId, eventJson, getSnapsot))
                    return! loop ()
            }

            loop ())

    // TODO: Log unhandled errors (via agent.Error.Add)...

    interface IReader with
        member _.ReadAsync guid =
            agent.PostAndAsyncReply(fun reply -> Read(guid, reply))

        member _.ReadAllAsync() =
            agent.PostAndAsyncReply(fun reply -> ReadAll reply)

    interface IWriter with
        member _.WriteAsync(guid, rvn, auditUserId, eventJson, getSnapshot) =
            agent.PostAndAsyncReply(fun reply -> Write(guid, rvn, auditUserId, eventJson, getSnapshot, reply))
