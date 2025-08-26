namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open System

type Source =
    | User of userId: UserId
    | System of sanitizedTypeName: string

type Entry =
    | EventJson of rvn: Rvn * timestampUtc: DateTime * source: Source * json: Json
    | SnapshotJson of rvn: Rvn * json: Json

    member this.Rvn =
        match this with
        | EventJson(rvn, _, _, _) -> rvn
        | SnapshotJson(rvn, _) -> rvn

type GetSnapshot = unit -> Json

type PartitionName = string
type EntityName = string

type IReader =
    abstract ReadAllAsync: unit -> Async<Result<(Guid * NonEmptyList<Entry>) list, string list>>

type IWriter =
    // TODO-PERSISTENCE...abstract ArchiveAllAsync: (Guid * NonEmptyList<Entry> -> Rvn * Json) -> Async<Result<unit, string list>>
    abstract CreateFromSnapshotAsync: Guid * Rvn * Json -> Async<Result<unit, string>>
    abstract WriteEventAsync: Guid * Rvn * Source * IEvent * GetSnapshot option -> Async<Result<unit, string>>

type IPersistenceClock =
    abstract GetUtcNow: unit -> DateTime

type PersistenceClock() =
    interface IPersistenceClock with
        member _.GetUtcNow() = DateTime.UtcNow

type IPersistenceFactory =
    abstract GetReader<'state, 'event when 'state :> IState<'state, 'event>> : PartitionName option -> IReader
    abstract GetWriter<'state, 'event when 'state :> IState<'state, 'event>> : PartitionName option -> IWriter
