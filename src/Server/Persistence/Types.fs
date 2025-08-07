namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain

open System

type Entry =
    | EventJson of rvn: Rvn * timestampUtc: DateTime * auditUserId: UserId * json: Json
    | SnapshotJson of rvn: Rvn * json: Json

    member this.Rvn =
        match this with
        | EventJson(rvn, _, _, _) -> rvn
        | SnapshotJson(rvn, _) -> rvn

type GetSnapshot = unit -> Json

type PartitionKey = string
type EntityKey = string

type IReader =
    abstract ReadAsync: Guid -> Async<Result<Entry list, string>>
    abstract ReadAllAsync: unit -> Async<Result<Entry list, string> list>

type IWriter =
    abstract WriteAsync: Guid * Rvn * UserId * Json * GetSnapshot -> Async<Result<unit, string>>

type IPersistenceClock =
    abstract GetUtcNow: unit -> DateTime

type IPersistenceFactory =
    abstract GetReader: PartitionKey option * EntityKey -> IReader
    abstract GetWriter: PartitionKey option * EntityKey -> IWriter
