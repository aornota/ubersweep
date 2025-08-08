namespace Aornota.Ubersweep.Shared.Domain

open Aornota.Ubersweep.Shared

open System

type EntityId<'entity> =
    private
    | Id' of guid: Guid

    static member Initialize guid =
        match guid with
        | Some guid -> Id' guid
        | None -> Id'(Guid.NewGuid())

    member this.Guid =
        let (Id' guid) = this
        guid

type ToJson<'a> = 'a -> Json

type IState<'a> =
    abstract member SnapshotJson: ToJson<'a> -> Json

type IEvent<'a> =
    abstract member EventJson: ToJson<'a> -> Json
