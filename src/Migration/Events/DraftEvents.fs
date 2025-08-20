namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common

open System
open System.Collections.Generic

type DraftEvent =
    | DraftCreated of draftId: DraftId * draftOrdinal: DraftOrdinal * draftType: DraftType
    | DraftOpened of draftId: DraftId
    | DraftPendingProcessing of draftId: DraftId
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
    | DraftProcessed of draftId: DraftId
    | DraftFreeSelection of draftId: DraftId
    | FreePick of draftId: DraftId * draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset

type Draft = {
    DraftOrdinal: DraftOrdinal
    DraftStatus: DraftStatus
    DraftPicks: (DraftPick * PickedBy) list
    ProcessingEvents: ProcessingEvent list
    PickPriorities: Dictionary<UserId, uint32>
}

type DraftHelper() =
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
        | Some(draft, rvn), DraftOpened _ :: t ->
            match draft.DraftStatus with
            | PendingOpen(_, ends) -> applyEvents t (Some({ draft with DraftStatus = Opened ends }, rvn.NextRvn))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof DraftOpened} when not {nameof PendingOpen}"
        | Some(draft, rvn), DraftPendingProcessing _ :: t ->
            match draft.DraftStatus with
            | Opened _ ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus = PendingProcessing
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof DraftPendingProcessing} when not {nameof Opened}"
        | Some(draft, rvn), ProcessingStarted(_, seed) :: t ->
            match draft.DraftStatus with
            | PendingProcessing ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus = Processing
                                ProcessingEvents = ProcessingEvent.ProcessingStarted seed :: draft.ProcessingEvents
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof ProcessingStarted} when not {nameof PendingProcessing}"
        | Some(draft, rvn), WithdrawnPlayersIgnored(_, ignored) :: t ->
            match draft.DraftStatus with
            | Processing ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                ProcessingEvents =
                                    ProcessingEvent.WithdrawnPlayersIgnored ignored :: draft.ProcessingEvents
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof WithdrawnPlayersIgnored} when not {nameof Processing}"
        | Some(draft, rvn), RoundStarted(_, round) :: t ->
            match draft.DraftStatus with
            | Processing ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                ProcessingEvents = ProcessingEvent.RoundStarted round :: draft.ProcessingEvents
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof RoundStarted} when not {nameof Processing}"
        | Some(draft, rvn), AlreadyPickedIgnored(_, ignored) :: t ->
            match draft.DraftStatus with
            | Processing ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                ProcessingEvents =
                                    ProcessingEvent.AlreadyPickedIgnored ignored :: draft.ProcessingEvents
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof AlreadyPickedIgnored} when not {nameof Processing}"
        | Some(draft, rvn), NoLongerRequiredIgnored(_, ignored) :: t ->
            match draft.DraftStatus with
            | Processing ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                ProcessingEvents =
                                    ProcessingEvent.NoLongerRequiredIgnored ignored :: draft.ProcessingEvents
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof NoLongerRequiredIgnored} when not {nameof Processing}"
        | Some(draft, rvn), UncontestedPick(_, draftPick, userId) :: t ->
            match draft.DraftStatus with
            | Processing ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                ProcessingEvents =
                                    ProcessingEvent.UncontestedPick(draftPick, userId) :: draft.ProcessingEvents
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof UncontestedPick} when not {nameof Processing}"
        | Some(draft, rvn), ContestedPick(_, draftPick, userDetails, winner) :: t ->
            match draft.DraftStatus with
            | Processing ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                ProcessingEvents =
                                    ProcessingEvent.ContestedPick(draftPick, userDetails, winner)
                                    :: draft.ProcessingEvents
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof ContestedPick} when not {nameof Processing}"
        | Some(draft, rvn), PickPriorityChanged(_, userId, pickPriority) :: t ->
            match draft.DraftStatus with
            | Processing ->
                if draft.PickPriorities.ContainsKey userId then
                    draft.PickPriorities[userId] <- pickPriority
                else
                    draft.PickPriorities.Add(userId, pickPriority)

                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                ProcessingEvents =
                                    ProcessingEvent.PickPriorityChanged(userId, pickPriority)
                                    :: draft.ProcessingEvents
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof PickPriorityChanged} when not {nameof Processing}"
        | Some(draft, rvn), Picked(_, draftOrdinal, draftPick, userId, timestamp) :: t ->
            match draft.DraftStatus with
            | Processing ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftPicks = (draftPick, (userId, Some draftOrdinal, timestamp)) :: draft.DraftPicks
                                ProcessingEvents =
                                    ProcessingEvent.Picked(draftOrdinal, draftPick, userId, timestamp)
                                    :: draft.ProcessingEvents
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof Picked} when not {nameof Processing}"
        | Some(draft, rvn), DraftProcessed _ :: t ->
            match draft.DraftStatus with
            | Processing -> applyEvents t (Some({ draft with DraftStatus = Processed }, rvn.NextRvn))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof DraftProcessed} when not {nameof Processing}"
        | Some(draft, rvn), DraftFreeSelection _ :: t ->
            match draft.DraftStatus with
            | PendingFreeSelection ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus = FreeSelection
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error $"Invalid {nameof DraftEvent}: {nameof DraftFreeSelection} when not {nameof PendingFreeSelection}"
        | Some(draft, rvn), FreePick(_, draftPick, userId, timestamp) :: t ->
            match draft.DraftStatus with
            | FreeSelection ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftPicks = (draftPick, (userId, None, timestamp)) :: draft.DraftPicks
                        },
                        rvn.NextRvn
                    ))
            | _ -> Error $"Invalid {nameof DraftEvent}: {nameof FreePick} when not {nameof FreeSelection}"
        | Some(draft, rvn), [] ->
            Ok(
                {
                    draft with
                        DraftPicks = draft.DraftPicks |> List.rev
                        ProcessingEvents = draft.ProcessingEvents |> List.rev
                },
                rvn
            )
        | None, [] -> Error $"No initial {nameof DraftEvent}"
        | None, h :: _ -> Error $"Invalid initial {nameof DraftEvent}: {h}"
        | Some _, DraftCreated _ :: _ -> Error $"Invalid non-initial {nameof DraftEvent}: {nameof DraftCreated}"

    interface IHelper<DraftEvent, Draft> with
        member _.ApplyEvents events = applyEvents events None
