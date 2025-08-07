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

type IState =
    abstract member SnapshotJson: Json

type IEvent =
    abstract member EventJson: Json

#nowarn "3535"

type IEntity<'entity, 'state, 'event> when 'state :> IState and 'event :> IEvent =
    static abstract member Initialize: Guid option -> 'entity
    static abstract member Make: Guid * Rvn * 'state -> 'entity
    static abstract member Evolve: 'entity -> 'event -> 'entity
    abstract member Id: EntityId<'entity>
    abstract member Rvn: Rvn
    abstract member State: 'state
