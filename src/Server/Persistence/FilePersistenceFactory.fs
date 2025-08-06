module Aornota.Ubersweep.Server.Persistence.FilePersistenceFactory

open Aornota.Ubersweep.Server.Persistence.FileReaderAndWriter
open Aornota.Ubersweep.Server.Persistence.Types

open Microsoft.Extensions.Configuration
open System.Collections.Concurrent

// TODO: Add ILogger...
type FilePersistenceFactory(config: IConfiguration) =
    let root = @"D:\UberSweep\ubersweep\src\Server\persisted" // TODO: Get from IConfiguration...
    let snapshotFrequency = Some 5u // TODO: Get from IConfiguration...

    let dic = ConcurrentDictionary<PartitionKey * EntityKey, IReader * IWriter>()

    let getOrAdd (partitionKey, entityKey) =
        dic.GetOrAdd(
            (partitionKey, entityKey),
            (fun _ ->
                let readerAndWriter =
                    FileReaderAndWriter(root, partitionKey, entityKey, snapshotFrequency)

                readerAndWriter :> IReader, readerAndWriter :> IWriter)
        )

    interface IPersistenceFactory with
        member _.GetReader(partitionKey, entityKey) =
            getOrAdd (partitionKey, entityKey) |> fst

        member _.GetWriter(partitionKey, entityKey) =
            getOrAdd (partitionKey, entityKey) |> snd
