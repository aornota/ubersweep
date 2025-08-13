namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Shared.Entities
open Aornota.Ubersweep.Shared.Common

open System

type IEntity =
    abstract SnapshotJson: Json

type private EntityInner<'id, 'entity when 'id :> IId and 'entity :> IEntity> = {
    Id: EntityId<'id>
    Rvn: Rvn
    State: 'entity
}

type Entity<'id, 'entity when 'id :> IId and 'entity :> IEntity and 'entity: equality>
    (id: EntityId<'id>, rvn: Rvn, state: 'entity) =
    let mutable entity = { Id = id; Rvn = rvn; State = state }

    override this.Equals other =
        match other with
        | :? Entity<'id, 'entity> as other -> this.Equals other
        | _ -> false

    override _.GetHashCode() = entity.GetHashCode()

    member _.Equals(other: Entity<'id, 'entity>) =
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

    interface IEquatable<Entity<'id, 'entity>> with
        member this.Equals other = this.Equals other

type IEvent =
    abstract EventJson: Json
