namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type SweepstakeId =
    private
    | SweepstakeId of guid: Guid

    static member Create() = SweepstakeId(Guid.NewGuid())
    static member FromGuid guid = SweepstakeId guid

    interface IId with
        member this.Guid =
            let (SweepstakeId guid) = this
            guid

// TODO-ENTITIES: More (e.g. scoring | payouts | &c.)...

type SweepstakeType =
    | Euro
    | Fifa
    // TODO-ENTITIES: FifaV2 (e.g. 48 squads?)...
    | Rwc

    member this.NameForPartition =
        match this with
        | Euro -> "euro"
        | Fifa -> "fifa"
        | Rwc -> "rwc"

    member this.SquadsPerGroup =
        match this with
        | Euro
        | Fifa
        | Rwc -> 4u

type SweepstakeStatus =
    | Pending // TODO-ENTITIES: Only visible to SuperUsers?...
    | Active
    | Archived // TODO-ENTITIES: Read-only?...

type SweepstakeInitCommand =
    | CreateSweepstake of
        sweepstakeType: SweepstakeType *
        year: uint32 *
        description: string *
        logo: string *
        maxPlayersPerSquad: uint32 *
        status: SweepstakeStatus

type SweepstakeCommand =
    (* TODO-ENTITIES?...
    | Deactivate
    | Unarchive
    *)
    | Activate
    | Archive

type SweepstakeCommon' = {
    SweepstakeType: SweepstakeType
    Year: uint32
    Description: string
    Logo: string
    MaxPlayersPerSquad: uint32
    Status: SweepstakeStatus
} with

    member this.PartitionName = $"{this.Year}-{this.SweepstakeType.NameForPartition}"
