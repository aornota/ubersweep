namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open Serilog
open System
open System.IO

type TestPersistenceDirectory<'id, 'state, 'initEvent, 'event
    when 'id :> IId and 'state :> IState<'state, 'event> and 'initEvent :> IEvent and 'event :> IEvent>
    (partitionName: PartitionName option, snapshotFrequency: uint option, ?retainOnDispose, ?skipCreatingDir) =
    let retainOnDispose = defaultArg retainOnDispose false
    let skipCreatingDir = defaultArg skipCreatingDir false

    let root =
        Path.Combine(AppDomain.CurrentDomain.BaseDirectory, Guid.NewGuid().ToString())

    let entityName: EntityName = typeof<'state>.Name

    let subPath =
        match partitionName with
        | Some partitionName -> Path.Combine(partitionName, entityName)
        | None -> entityName

    let path = Path.Combine(root, subPath)

    let pathForError = $"...\{DirectoryInfo(root).Name}\{subPath}"

    let dir = DirectoryInfo path

    do
        if not (skipCreatingDir || dir.Exists) then
            dir.Create()

    let agent =
        new FileReaderAndWriter(root, partitionName, entityName, snapshotFrequency, FixedClock.instance, Log.Logger)

    let reader, writer = agent :> IReader, agent :> IWriter

    member _.Path = path
    member _.PathForError = pathForError
    member _.ReadAsync guid = reader.ReadAsync guid
    member _.ReadAllAsync() = reader.ReadAllAsync()

    member _.CreateFromSnapshotAsync(guid, rvn, snapshotJson) =
        writer.CreateFromSnapshotAsync(guid, rvn, snapshotJson)

    member _.WriteEventAsync(entity: Entity<'id, 'state, 'event>, event: 'initEvent, auditUserId: UserId) =
        writer.WriteEventAsync(entity.Guid, entity.Rvn, auditUserId, event, Some(fun _ -> entity.SnapshotJson))

    member _.WriteEventAsync(entity: Entity<'id, 'state, 'event>, event: 'event, auditUserId: UserId) =
        writer.WriteEventAsync(entity.Guid, entity.Rvn, auditUserId, event, Some(fun _ -> entity.SnapshotJson))

    member _.WriteEventAsync(guid, rvn, event: 'initEvent, auditUserId: UserId, snapshotJson) =
        writer.WriteEventAsync(guid, rvn, auditUserId, event, Some(fun _ -> snapshotJson))

    member _.WriteEventAsync(guid, rvn, event: 'event, auditUserId: UserId, snapshotJson) =
        writer.WriteEventAsync(guid, rvn, auditUserId, event, Some(fun _ -> snapshotJson))

    member _.TryReadAllLinesAsync(guid: Guid) = asyncResult {
        try
            let file =
                FileInfo(Path.Combine(path, $"{guid}.{FileReaderAndWriter.FileExtension}"))

            if not (File.Exists file.FullName) then
                return! Error $"File {file.Name} does not exist"

            let! lines = File.ReadAllLinesAsync file.FullName

            return lines |> List.ofArray
        with exn ->
            return! Error $"Unexpected error reading all lines for {guid}: {exn.Message}"
    }

    member _.TryWriteAllLinesAsync(name: string, lines: string list) = asyncResult {
        try
            let file =
                FileInfo(Path.Combine(path, $"{name}.{FileReaderAndWriter.FileExtension}"))

            if File.Exists file.FullName then
                return! Error $"File {file.Name} already exists"

            return! File.WriteAllLinesAsync(file.FullName, lines)
        with exn ->
            return! Error $"Unexpected error writing all lines for {name}: {exn.Message}"
    }

    member this.TryWriteAllLinesAsync(guid: Guid, lines) =
        this.TryWriteAllLinesAsync(guid.ToString(), lines)

    interface IDisposable with
        member _.Dispose() : unit =
            (agent :> IDisposable).Dispose()

            if not retainOnDispose then
                let root = DirectoryInfo root

                if root.Exists then
                    root.Delete true
