namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Domain

open System

type DraftEvent =
    | DraftCreated of draftId: DraftId * draftOrdinal: DraftOrdinal * draftType: DraftType
    | DraftOpened of draftId: DraftId
    | DraftPendingProcessing of draftId: DraftId
    | DraftProcessed of draftId: DraftId
    | DraftFreeSelection of draftId: DraftId
    | ProcessingStarted of draftId: DraftId * seed: int
    | WithdrawnPlayersIgnored of draftId: DraftId * ignored: (UserId * (SquadId * PlayerId) list) list
    | RoundStarted of draftId: DraftId * round: uint32
    | AlreadyPickedIgnored of draftId: DraftId * ignored: (UserId * DraftPick list) list
    | NoLongerRequiredIgnored of draftId: DraftId * ignored: (UserId * DraftPick list) list
    | UncontestedPick of draftId: DraftId * draftPick: DraftPick * userId: UserId
    | ContestedPick of
        draftId: DraftId *
        draftPick: DraftPick *
        userDetails: (UserId * uint32 * float option) list *
        winner: UserId
    | PickPriorityChanged of draftId: DraftId * userId: UserId * pickPriority: uint32
    | Picked of
        draftId: DraftId *
        draftOrdinal: DraftOrdinal *
        draftPick: DraftPick *
        userId: UserId *
        timestamp: DateTimeOffset
    | FreePick of draftId: DraftId * draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset
