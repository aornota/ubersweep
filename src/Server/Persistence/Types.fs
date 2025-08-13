namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open System

type Entry =
    | EventJson of rvn: Rvn * timestampUtc: DateTime * auditUserId: EntityId<UserId> * json: Json
    | SnapshotJson of rvn: Rvn * json: Json

    member this.Rvn =
        match this with
        | EventJson(rvn, _, _, _) -> rvn
        | SnapshotJson(rvn, _) -> rvn

type GetSnapshot = unit -> Json

type PartitionName = string
type EntityName = string

type IReader =
    abstract ReadAsync: Guid -> Async<Result<NonEmptyList<Entry>, string>>
    abstract ReadAllAsync: unit -> Async<Result<Guid * NonEmptyList<Entry>, string> list>

type IWriter =
    abstract WriteAsync: Guid * Rvn * EntityId<UserId> * Json * GetSnapshot -> Async<Result<unit, string>>

type IPersistenceClock =
    abstract GetUtcNow: unit -> DateTime

type PersistenceClock() =
    interface IPersistenceClock with
        member _.GetUtcNow() = DateTime.UtcNow

type IPersistenceFactory =
    abstract GetReader<'a> : PartitionName option -> IReader

    abstract GetWriter<'a> : PartitionName option -> IWriter
