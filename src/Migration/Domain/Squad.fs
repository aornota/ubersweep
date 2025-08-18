namespace Aornota.Ubersweep.Migration.Domain

open System

type SquadId =
    | SquadId of guid: Guid

    static member Create() = Guid.NewGuid() |> SquadId

type Seeding = Seeding of seeding: int

type PlayerId =
    | PlayerId of guid: Guid

    static member Create() = Guid.NewGuid() |> PlayerId

type PlayerTypeFootball =
    | Goalkeeper
    | Defender
    | Midfielder
    | Forward

type PlayerTypeRugby =
    | Forward
    | Back

type PlayerStatus =
    | Active
    | Withdrawn of dateWithdrawn: DateTimeOffset option

type PickedBy = UserId * DraftOrdinal option * DateTimeOffset
