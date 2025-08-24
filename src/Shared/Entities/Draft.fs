namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type DraftId =
    private
    | DraftId of guid: Guid

    static member Create() = DraftId(Guid.NewGuid())
    static member FromGuid guid = DraftId guid

    interface IId with
        member this.Guid =
            let (DraftId guid) = this
            guid

type DraftPick =
    | TeamPicked of squadId: SquadId
    | PlayerPicked of squadId: SquadId * playerId: PlayerId

type PickedBy = UserId * DraftOrdinal option * DateTimeOffset

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
    | Processing
    | Processed of
        draftPicks: (DraftPick * PickedBy) list *
        processingEvents: ProcessingEvent list *
        pickPriorities: Map<UserId, uint32>

type UnconstrainedStatus =
    // TODO-ENTITIES: Is PendingFreeSelection needed?...
    | PendingFreeSelection
    | FreeSelection of draftPicks: (DraftPick * PickedBy) list

type DraftStatus =
    | ConstrainedDraft of draftOrdinal: DraftOrdinal * constrainedStatus: ConstrainedStatus
    | UnconstrainedDraft of unconstrainedStatus: UnconstrainedStatus

type DraftType =
    | Constrained of draftOrdinal: DraftOrdinal * starts: DateTimeOffset * ends: DateTimeOffset
    | Unconstrained

type DraftInitCommand = CreateDraft of draftType: DraftType

type DraftCommand =
    (* TODO-ENTITIES?...
    | ChangeStarts of starts: DateTimeOffset
    | ChangeEnds of ends: DateTimeOffset
    *)
    | OpenDraft
    | CloseDraft
    | StartProcessing
    | FinishProcesing of
        draftPicks: (DraftPick * PickedBy) list *
        processingEvents: ProcessingEvent list *
        pickPriorities: Map<UserId, uint32>
    | OpenFreeSelection
    | MakeFreePick of draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset

type DraftCommon' = { DraftStatus: DraftStatus }
