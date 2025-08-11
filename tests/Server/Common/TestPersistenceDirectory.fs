namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Domain

open System
open System.IO

type TestPersistenceDirectory<'entity, 'state when 'entity :> Entity<'state> and 'state :> IState and 'state: equality>
    (partitionKey: PartitionKey option, snapshotFrequency: uint option, ?retainOnDispose) =
    let retainOnDispose = defaultArg retainOnDispose false

    let root =
        Path.Combine(AppDomain.CurrentDomain.BaseDirectory, Guid.NewGuid().ToString())

    let entityKey: EntityKey = typeof<'entity>.Name

    let path =
        match partitionKey with
        | Some partitionKey -> Path.Combine(root, partitionKey, entityKey)
        | None -> Path.Combine(root, entityKey)

    let dir = DirectoryInfo path

    do
        if not dir.Exists then
            dir.Create()

    let agent =
        new FileReaderAndWriter(root, partitionKey, entityKey, snapshotFrequency, FixedClock.instance)

    let reader, writer = agent :> IReader, agent :> IWriter

    member _.ReadAsync guid = reader.ReadAsync guid
    member _.ReadAllAsync() = reader.ReadAllAsync()

    member _.WriteAsync<'event when 'event :> IEvent>
        (entity: IEntity<'state>, event: 'event, auditUserId: EntityId<UserState>)
        =
        writer.WriteAsync(entity.Id.Guid, entity.Rvn, auditUserId, event.EventJson, (fun _ -> entity.SnapshotJson))

    // TODO: Add methods to read/write "raw" files?...

    interface IDisposable with
        member _.Dispose() : unit =
            (agent :> IDisposable).Dispose()

            if not retainOnDispose then
                let root = DirectoryInfo root

                if root.Exists then
                    root.Delete true
