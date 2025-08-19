namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System

type DraftInitEvent =
    | DraftCreated of draftOrdinal: DraftOrdinal * draftType: DraftType

    interface IEvent with
        member this.EventJson = Json.toJson this

type DraftEvent =
    | DraftOpened
    | DraftPendingProcessing
    | ProcessingStarted of seed: int
    | WithdrawnPlayersIgnored of ignored: (UserId * (SquadId * PlayerId) list) list
    | RoundStarted of round: uint32
    | AlreadyPickedIgnored of ignored: (UserId * DraftPick list) list
    | NoLongerRequiredIgnored of ignored: (UserId * DraftPick list) list
    | UncontestedPick of draftPick: DraftPick * userId: UserId
    | ContestedPick of draftPick: DraftPick * userDetails: (UserId * uint32 * float option) list * winner: UserId
    | PickPriorityChanged of userId: UserId * pickPriority: uint32
    | Picked of draftOrdinal: DraftOrdinal * draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset
    | DraftProcessed
    | DraftFreeSelection
    | FreePick of draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset

    interface IEvent with
        member this.EventJson = Json.toJson this

type Draft = { // TODO: Implement this properly...
    Dummy: unit
} with

    interface IState<Draft, DraftEvent> with
        member this.SnapshotJson = Json.toJson this

        member this.Evolve event = this
