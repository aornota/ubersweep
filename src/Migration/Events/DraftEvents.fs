namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common

open System
open System.Collections.Generic

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

type Draft = {
    DraftOrdinal: DraftOrdinal
    DraftStatus: DraftStatus
    DraftPicks: (DraftPick * PickedBy) list
    ProcessingEvents: ProcessingEvent list
    PickPriorities: Dictionary<UserId, uint32>
}

type DraftHelper() =
    // TODO: How best to handle lists (e.g. add to head, then reverse when returning)?...
    let rec applyEvents (events: DraftEvent list) draftsAndRvn =
        match draftsAndRvn, events with
        | None, DraftCreated(_, draftOrdinal, draftType) :: t ->
            applyEvents
                t
                (Some(
                    {
                        DraftOrdinal = draftOrdinal
                        DraftStatus =
                            match draftType with
                            | Constrained(starts, ends) -> PendingOpen(starts, ends)
                            | Unconstrained -> PendingFreeSelection
                        DraftPicks = []
                        ProcessingEvents = []
                        PickPriorities = Dictionary<UserId, uint32>()
                    },
                    Rvn.InitialRvn
                ))
        // TODO: DraftOpened...
        // TODO: DraftPendingProcessing...
        // TODO: DraftProcessed...
        // TODO: DraftFreeSelection...
        // TODO: ProcessingStarted...
        // TODO: WithdrawnPlayersIgnored...
        // TODO: RoundStarted...
        // TODO: AlreadyPickedIgnored...
        // TODO: NoLongerRequiredIgnored...
        // TODO: UncontestedPick...
        // TODO: ContestedPick...
        // TODO: PickPriorityChanged...
        // TODO: Picked...
        // TODO: FreePick...
        | Some draftsAndRvn, [] -> Ok draftsAndRvn
        | None, [] -> Error $"No initial {nameof DraftEvent}"
        | None, h :: _ -> Error $"Invalid initial {nameof DraftEvent}: {h}"
        | Some _, DraftCreated _ :: _ -> Error $"Invalid non-initial {nameof DraftEvent}: {nameof DraftCreated}"

    interface IHelper<DraftEvent, Draft> with
        member _.ApplyEvents events = applyEvents events None
