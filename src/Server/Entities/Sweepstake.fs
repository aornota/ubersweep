namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling

type SweepstakeInitEvent =
    | SweepstakeCreated of
        sweepstakeType: SweepstakeType *
        year: uint32 *
        description: string *
        logo: string *
        maxPlayersPerSquad: uint32 *
        status: SweepstakeStatus

    interface IEvent with
        member this.EventJson = Json.encode this

type SweepstakeEvent =
    | Activated
    | Archived

    interface IEvent with
        member this.EventJson = Json.encode this

type Sweepstake = {
    SweepstakeCommon: SweepstakeCommon'
} with

    interface IState<Sweepstake, SweepstakeEvent> with
        member this.SnapshotJson = Json.encode this

        member this.Evolve event =
            match event with
            | Activated ->
                Ok {
                    this with
                        SweepstakeCommon.Status = SweepstakeStatus.Active
                }
            | Archived ->
                Ok {
                    this with
                        SweepstakeCommon.Status = SweepstakeStatus.Archived
                }

type SweepstakeHelper() =
    inherit EntityHelper<SweepstakeId, Sweepstake, SweepstakeInitCommand, SweepstakeInitEvent, SweepstakeEvent>()

    override _.IdFromGuid guid = SweepstakeId.FromGuid guid

    override _.InitFromCommand
        (guid, CreateSweepstake(sweepstakeType, year, description, logo, maxPlayersPerSquad, status))
        =
        {
            Id = SweepstakeId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = {
                SweepstakeCommon = {
                    SweepstakeType = sweepstakeType
                    Year = year
                    Description = description
                    Logo = logo
                    MaxPlayersPerSquad = maxPlayersPerSquad
                    Status = status
                }
            }
        },
        SweepstakeCreated(sweepstakeType, year, description, logo, maxPlayersPerSquad, status)

    override _.InitFromEvent
        (guid, SweepstakeCreated(sweepstakeType, year, description, logo, maxPlayersPerSquad, status))
        =
        {
            Id = SweepstakeId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = {
                SweepstakeCommon = {
                    SweepstakeType = sweepstakeType
                    Year = year
                    Description = description
                    Logo = logo
                    MaxPlayersPerSquad = maxPlayersPerSquad
                    Status = status
                }
            }
        }

[<RequireQualifiedAccess>]
module Sweepstake =
    let private decide command (sweepstake: Sweepstake) =
        match command with
        | Activate ->
            match sweepstake.SweepstakeCommon.Status with
            | Pending -> Ok Activated
            | _ -> Error $"{nameof Activate} when {nameof Sweepstake} not {nameof Pending}"
        | Archive ->
            match sweepstake.SweepstakeCommon.Status with
            | SweepstakeStatus.Active -> Ok Archived
            | _ -> Error $"{nameof Archive} when {nameof Sweepstake} not {nameof SweepstakeStatus.Active}"

    let helper = SweepstakeHelper()

    let apply command (entity: Entity<SweepstakeId, Sweepstake, SweepstakeEvent>) = result {
        let! event = decide command entity.State
        let! entity = entity.Evolve event
        return entity, event
    }
