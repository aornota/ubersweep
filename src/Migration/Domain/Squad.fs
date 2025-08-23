namespace Aornota.Ubersweep.Migration.Domain

open Aornota.Ubersweep.Shared.Common

open System

type SquadId =
    | SquadId of guid: Guid

    static member Create() = Guid.NewGuid() |> SquadId

type PlayerId =
    | PlayerId of guid: Guid

    static member Create() = Guid.NewGuid() |> PlayerId

type PickedBy = UserId * DraftOrdinal option * DateTimeOffset
