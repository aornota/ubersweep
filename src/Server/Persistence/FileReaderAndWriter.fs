namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open Serilog
open System
open System.IO

(* TODO-PERSISTENCE:
    -- Implement here initially...
    -- ...but then move (as private type) to FilePersistenceFactory.fs (and rename that as FilePersistence.fs?)... *)

module private FileReaderAndWriter =
    // TODO-PERSISTENCE: tryParseDir | tryRead | tryReadAll | tryWrite | ...

    let _ = () // TEMP

// TODO-PERSISTENCE...type private Input =

type private FileReaderAndWriter
    (
        root: string,
        partitionName: PartitionName option,
        entityName: EntityName,
        snapshotFrequency: uint option,
        clock: IPersistenceClock,
        logger: ILogger
    ) =
    [<Literal>]
    static let eventsFileExtension = "events"

    [<Literal>]
    static let snapshotFileExtension = "snapshot"

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

    // TODO-PERSISTENCE...let agent =

    // TODO-PERSISTENCE...do agent.Error.Add(fun exn -> logger.Error("Unexpected error: {message} at {target}", exn.Message, exn.TargetSite))

    static member EventsFileExtension = eventsFileExtension
    static member SnapshotFileExtension = snapshotFileExtension

    // TODO-PERSISTENCE...interface IReader with

    // TODO-PERSISTENCE...interface IWriter with

    interface IDisposable with
        member _.Dispose() =
            // TODO-PERSISTENCE...agent.Dispose()
            logger.Warning "Disposed"
