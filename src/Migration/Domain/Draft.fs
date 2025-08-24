namespace Aornota.Ubersweep.Migration.Domain

open Aornota.Ubersweep.Shared.Common

open System
open System.Collections.Generic

type DraftId =
    | DraftId of guid: Guid

    static member Create() = Guid.NewGuid() |> DraftId

type DraftPick =
    | TeamPicked of squadId: SquadId
    | PlayerPicked of squadId: SquadId * playerId: PlayerId

type PickedBy' = UserId * DraftOrdinal option * DateTimeOffset

type ProcessingEvent =
    | ProcessingStarted of seed: int
    | WithdrawnPlayersIgnored of ignored: (UserId * (SquadId * PlayerId) list) list
    | RoundStarted of round: uint32
    | AlreadyPickedIgnored of ignored: (UserId * DraftPick list) list
    | NoLongerRequiredIgnored of ignored: (UserId * DraftPick list) list
    | UncontestedPick of draftPick: DraftPick * userId: UserId
    | ContestedPick of draftPick: DraftPick * userDetails: (UserId * uint32 * float option) list * winner: UserId
    | PickPriorityChanged of userId: UserId * pickPriority: uint32
    | Picked of draftOrdinal: DraftOrdinal * draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset

type ConstrainedStatus =
    | PendingOpen of starts: DateTimeOffset * ends: DateTimeOffset
    | Opened of ends: DateTimeOffset
    | PendingProcessing
    | Processing of
        draftPicks: (DraftPick * PickedBy') list *
        processingEvents: ProcessingEvent list *
        pickPriorities: Dictionary<UserId, uint32>
    | Processed of
        draftPicks: (DraftPick * PickedBy') list *
        processingEvents: ProcessingEvent list *
        pickPriorities: Dictionary<UserId, uint32>

type UnconstrainedStatus =
    | PendingFreeSelection
    | FreeSelection of draftPicks: (DraftPick * PickedBy) list

type DraftStatus =
    | ConstrainedDraft of draftOrdinal: DraftOrdinal * constrainedStatus: ConstrainedStatus
    | UnconstrainedDraft of unconstrainedStatus: UnconstrainedStatus

type DraftType =
    | Constrained of starts: DateTimeOffset * ends: DateTimeOffset
    | Unconstrained
