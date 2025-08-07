namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence

open System
open System.IO

type TestDirectory(partitionKey: PartitionKey option, entityKey: EntityKey, ?retainOnDispose) =
    let retainOnDispose = defaultArg retainOnDispose false

    let root =
        Path.Combine(AppDomain.CurrentDomain.BaseDirectory, Guid.NewGuid().ToString())

    let path =
        match partitionKey with
        | Some partitionKey -> Path.Combine(root, partitionKey, entityKey)
        | None -> Path.Combine(root, entityKey)

    let dir = DirectoryInfo path

    do
        if not dir.Exists then
            dir.Create()

    member _.PartitionKey = partitionKey
    member _.EntityKey = entityKey
    member _.Root = root
    member _.DirectoryInfo = dir

    interface IDisposable with
        member _.Dispose() : unit =
            let root = DirectoryInfo root

            if not retainOnDispose && root.Exists then
                root.Delete true
