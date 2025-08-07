namespace Aornota.Ubersweep.Server.Persistence

open Microsoft.Extensions.Configuration
open System.Collections.Concurrent

// TODO: Add ILogger...
type FilePersistenceFactory(config: IConfiguration, clock: IPersistenceClock) =
    let root = @"D:\UberSweep\ubersweep\src\Server\persisted" // TODO: Get from IConfiguration...
    let snapshotFrequency = Some 5u // TODO: Get from IConfiguration...

    let dic = ConcurrentDictionary<PartitionKey option * EntityKey, IReader * IWriter>()

    let getOrAdd (partitionKey, entityKey) =
        dic.GetOrAdd(
            (partitionKey, entityKey),
            (fun _ ->
                let readerAndWriter =
                    FileReaderAndWriter(root, partitionKey, entityKey, snapshotFrequency, clock)

                readerAndWriter :> IReader, readerAndWriter :> IWriter)
        )

    interface IPersistenceFactory with
        member _.GetReader(partitionKey, entityKey) =
            getOrAdd (partitionKey, entityKey) |> fst

        member _.GetWriter(partitionKey, entityKey) =
            getOrAdd (partitionKey, entityKey) |> snd
