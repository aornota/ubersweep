module Aornota.Ubersweep.Server.Persistence.Types

open Aornota.Ubersweep.Server.Persistence.ValidPathCharsString
open Aornota.Ubersweep.Shared.Json
open Aornota.Ubersweep.Shared.NonEmptyList
open Aornota.Ubersweep.Shared.Rvn

open System

type ReaderError =
    | FileDoesNotExist of path: string * guid: Guid
    | FileIsEmpty of path: string * guid: Guid
    | FileHasInconsistentRvns of path: string * guid: Guid
    | DirectoryDoesNotExist of path: string
    | FilesWithNonGuidNames of path: string * names: string list
    | OtherReaderError of exn: exn * path: string * guid: Guid option

type WriterError =
    | InitialRvnButFileAlreadyExists of path: string * guid: Guid
    | NotInitialRvnButFileDoesNotExist of path: string * guid: Guid * rvn: Rvn
    | NotInitialRvnButFileIsEmpty of path: string * guid: Guid * rvn: Rvn
    | RvnNotConsistentWithPreviousRvn of path: string * guid: Guid * rvn: Rvn * previousRvn: Rvn
    | OtherWriterError of exn: exn * path: string * guid: Guid * rvn: Rvn

type Entry =
    | EventJson of rvn: Rvn * json: Json // TODO: TimestampUtc and AuditUserId...
    | SnapshotJson of rvn: Rvn * json: Json

    member this.Rvn =
        match this with
        | EventJson(rvn, _) -> rvn
        | SnapshotJson(rvn, _) -> rvn

type GetSnapshot = unit -> Json

type PartitionKey = ValidPathCharsString
type EntityKey = ValidPathCharsString

type IReader =
    abstract ReadAsync: Guid -> Async<Result<NonEmptyList<Entry>, ReaderError>>
    abstract ReadAllAsync: unit -> Async<Result<NonEmptyList<Entry>, ReaderError> list>

type IWriter =
    abstract WriteAsync: Guid * Rvn * Json * GetSnapshot -> Async<Result<unit, WriterError>> // TODO: TimestampUtc...

type IPersistenceFactory =
    abstract GetReader: PartitionKey * EntityKey -> IReader
    abstract GetWriter: PartitionKey * EntityKey -> IWriter
