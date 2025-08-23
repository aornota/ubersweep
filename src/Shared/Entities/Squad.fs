namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type SquadId =
    private
    | SquadId of guid: Guid

    static member Create() = SquadId(Guid.NewGuid())
    static member FromGuid guid = SquadId guid

    interface IId with
        member this.Guid =
            let (SquadId guid) = this
            guid

type Seeding = Seeding of seeding: uint32

type PlayerId =
    private
    | PlayerId of guid: Guid

    static member Create() = PlayerId(Guid.NewGuid())
    static member FromGuid guid = PlayerId guid

    interface IId with
        member this.Guid =
            let (PlayerId guid) = this
            guid

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
