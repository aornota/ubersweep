namespace Aornota.Ubersweep.Server.Persistence

// TODO-PERSISTENCE: Remove this once new FileReaderAndWriter has been implemeneted (with tests)...

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open System

type Entry' =
    | EventJson' of rvn: Rvn * timestampUtc: DateTime * auditUserId: UserId * json: Json
    | SnapshotJson' of rvn: Rvn * json: Json

    member this.Rvn =
        match this with
        | EventJson'(rvn, _, _, _) -> rvn
        | SnapshotJson'(rvn, _) -> rvn

type IReader' =
    abstract ReadAsync': Guid -> Async<Result<NonEmptyList<Entry'>, string>>
    abstract ReadAllAsync': unit -> Async<Result<Guid * NonEmptyList<Entry'>, string> list>

type IWriter' =
    abstract CreateFromSnapshotAsync': Guid * Rvn * Json -> Async<Result<unit, string>>
    abstract WriteEventAsync': Guid * Rvn * UserId * IEvent * GetSnapshot option -> Async<Result<unit, string>>
