namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System

type SquadInitEvent<'group> =
    | SquadCreated of squadName: string * group: 'group * seeding: Seeding option * coachName: string

    interface IEvent with
        member this.EventJson = Json.toJson this

type SquadEvent<'playerType> =
    | PlayerAdded of playerId: PlayerId * playerName: string * playerType: 'playerType
    | PlayerNameChanged of playerId: PlayerId * playerName: string
    | PlayerTypeChanged of playerId: PlayerId * playerType: 'playerType
    | PlayerWithdrawn of playerId: PlayerId * dateWithdrawn: DateTimeOffset option
    | SquadEliminated

    interface IEvent with
        member this.EventJson = Json.toJson this

type Squad<'group, 'playerType> = { // TODO-ENTITIES: Implement this properly...
    Dummy: unit
} with

    interface IState<Squad<'group, 'playerType>, SquadEvent<'playerType>> with
        member this.SnapshotJson = Json.toJson this

        member this.Evolve event = this
