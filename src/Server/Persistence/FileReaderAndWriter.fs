module Aornota.Ubersweep.Server.Persistence.FileReaderAndWriter

open Aornota.Ubersweep.Server.Common.JsonConverter
open Aornota.Ubersweep.Server.Persistence.Types
open Aornota.Ubersweep.Shared.Json
open Aornota.Ubersweep.Shared.NonEmptyList
open Aornota.Ubersweep.Shared.Rvn

open System
open System.IO

type private Input =
    | Read of guid: Guid * reply: AsyncReplyChannel<Result<NonEmptyList<Entry>, ReaderError>>
    | ReadAll of reply: AsyncReplyChannel<Result<NonEmptyList<Entry>, ReaderError> list>
    | Write of
        guid: Guid *
        rvn: Rvn *
        eventJson: Json *
        getSnapsot: GetSnapshot *
        reply: AsyncReplyChannel<Result<unit, WriterError>>

// TODO: Add ILogger...
type FileReaderAndWriter(root: string, partitionKey: PartitionKey, entityKey: EntityKey, snapshotFrequency: uint option)
    =
    [<Literal>]
    let fileExtension = "entries"

    let path = Path.Combine(root, partitionKey.String, entityKey.String)

    do
        if not (Directory.Exists path) then
            Directory.CreateDirectory path |> ignore

    let tryRead guid =
        try
            let file = FileInfo(Path.Combine(path, $"{guid.ToString()}.{fileExtension}"))

            if not (File.Exists file.FullName) then
                Error(FileDoesNotExist(path, guid))
            else
                match File.ReadAllLines file.FullName |> List.ofArray with
                | [] -> Error(FileIsEmpty(path, guid))
                | lines ->
                    (* TODO: Implement this - and check for specific errors:
                        fromJson<Entry> errors
                        FileHasInconsistentRvns *)
                    let deserializableLines =
                        lines
                        |> List.choose (fun line ->
                            match fromJson<Entry> (Json line) with
                            | Ok entry -> Some entry
                            | Error _ -> None)

                    match deserializableLines with
                    | [] -> failwith $"TODO: No deserializable lines..."
                    | h :: t -> Ok(NonEmptyList<Entry>.Create(h, t))
        with exn ->
            Error(OtherReaderError(exn, path, Some guid))

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
                | h :: t -> [ Error(FilesWithNonGuidNames(path, h :: t |> List.map _.Name)) ]
                | _ -> files |> List.choose snd |> List.map tryRead
            else
                [ Error(DirectoryDoesNotExist path) ]
        with exn -> [ Error(OtherReaderError(exn, path, None)) ]

    let tryWrite (guid, rvn: Rvn, eventJson: Json, getSnapshot) =
        try
            let file = FileInfo(Path.Combine(path, $"{guid.ToString()}.{fileExtension}"))

            match rvn.IsInitialRvn, File.Exists file.FullName with
            | true, true -> Error(InitialRvnButFileAlreadyExists(path, guid))
            | false, false -> Error(NotInitialRvnButFileDoesNotExist(path, guid, rvn))
            | true, false ->
                let (Json eventJson') = toJson (EventJson(rvn, eventJson))
                File.WriteAllLines(file.FullName, [| eventJson' |])
                Ok()
            | false, true ->
                (* TODO: Implement this - and check for specific errors:
                    fromJson<Entry> errors *)
                match File.ReadAllLines file.FullName |> List.ofArray |> List.rev with
                | lastLine :: _ ->
                    match fromJson<Entry> (Json lastLine) with
                    | Ok entry ->
                        if rvn.IsValidNextRvn(Some entry.Rvn) then
                            let (Json eventJson') = toJson (EventJson(rvn, eventJson))
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
                            Error(RvnNotConsistentWithPreviousRvn(path, guid, rvn, entry.Rvn))
                    | Error error -> failwith error // TODO: Deserialization error...
                | [] -> Error(NotInitialRvnButFileIsEmpty(path, guid, rvn))
        with exn ->
            Error(OtherWriterError(exn, path, guid, rvn))

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
                | Write(guid, rvn, eventJson, getSnapsot, reply) ->
                    reply.Reply(tryWrite (guid, rvn, eventJson, getSnapsot))
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
        member _.WriteAsync(guid, rvn, eventJson, getSnapshot) =
            agent.PostAndAsyncReply(fun reply -> Write(guid, rvn, eventJson, getSnapshot, reply))
