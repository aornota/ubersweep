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

type IEntity =
    abstract SnapshotJson: Json

type private EntityInner<'entity when 'entity :> IEntity> = {
    Id: EntityId<'entity>
    Rvn: Rvn
    State: 'entity
}

type Entity<'entity when 'entity :> IEntity and 'entity: equality>(id: EntityId<'entity>, rvn: Rvn, state: 'entity) =
    let mutable entity = { Id = id; Rvn = rvn; State = state }

    override this.Equals other =
        match other with
        | :? Entity<'entity> as other -> this.Equals other
        | _ -> false

    override _.GetHashCode() = entity.GetHashCode()

    member _.Equals(other: Entity<'entity>) =
        entity.Id = other.Id && entity.Rvn = other.Rvn && entity.State = other.State

    member _.Id = entity.Id
    member _.Rvn = entity.Rvn
    member _.State = entity.State
    member _.SnapshotJson = Json.toJson entity.State

    member _.Evolve state =
        entity <- {
            entity with
                Rvn = entity.Rvn.NextRvn
                State = state
        }

    interface IEquatable<Entity<'entity>> with
        member this.Equals other = this.Equals other

type IEvent =
    abstract EventJson: Json
