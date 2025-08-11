namespace Aornota.Ubersweep.Shared.Domain

open Aornota.Ubersweep.Shared

type UserType =
    | SuperUser
    | Administrator
    | Pleb
    | PersonaNonGrata

type User = {
    // TODO: More fields...
    UserType: UserType
} with

    interface IEntity with
        member this.SnapshotJson = Json.toJson this
