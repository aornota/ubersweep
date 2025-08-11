namespace Aornota.Ubersweep.Shared.Domain

open Aornota.Ubersweep.Shared

open System

type EntityId<'entity> =
    private
    | Id of guid: Guid

    static member Initialize guid =
        match guid with
        | Some guid -> Id guid
        | None -> Id(Guid.NewGuid())

    member this.Guid =
        let (Id guid) = this
        guid

type IState =
    abstract SnapshotJson: Json

type IEntity<'state when 'state :> IState> =
    abstract Id: EntityId<'state>
    abstract Rvn: Rvn
    abstract SnapshotJson: Json
    abstract Evolve: 'state -> unit

type private EntityInner<'state when 'state :> IState> = {
    Id: EntityId<'state>
    Rvn: Rvn
    State: 'state
}

[<AbstractClass>]
type Entity<'state when 'state :> IState and 'state: equality>(id: EntityId<'state>, rvn: Rvn, state: 'state) =
    let mutable entity = { Id = id; Rvn = rvn; State = state }
    member _.State = entity.State

    member _.Equals(other: Entity<'state>) =
        entity.Id = (other :> IEntity<'state>).Id
        && entity.Rvn = (other :> IEntity<'state>).Rvn
        && entity.State = other.State

    override this.Equals other =
        match other with
        | :? Entity<'state> as other -> this.Equals other
        | _ -> false

    override _.GetHashCode() = entity.GetHashCode()

    interface IEntity<'state> with
        member _.Id = entity.Id
        member _.Rvn = entity.Rvn
        member _.SnapshotJson = Json.toJson entity.State

        member _.Evolve state =
            entity <- {
                entity with
                    Rvn = entity.Rvn.NextRvn
                    State = state
            }

    interface IEquatable<Entity<'state>> with
        member this.Equals other = this.Equals other

type IEvent =
    abstract EventJson: Json
