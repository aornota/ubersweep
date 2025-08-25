namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open System
open System.Collections.Generic

type DraftEvent' =
    | DraftCreated of draftId: DraftId' * draftOrdinal: DraftOrdinal * draftType: DraftType'
    | DraftOpened of draftId: DraftId'
    | DraftPendingProcessing of draftId: DraftId'
    | ProcessingStarted of draftId: DraftId' * seed: int
    | WithdrawnPlayersIgnored of draftId: DraftId' * ignored: (UserId' * (SquadId' * PlayerId') list) list
    | RoundStarted of draftId: DraftId' * round: uint32
    | AlreadyPickedIgnored of draftId: DraftId' * ignored: (UserId' * DraftPick' list) list
    | NoLongerRequiredIgnored of draftId: DraftId' * ignored: (UserId' * DraftPick' list) list
    | UncontestedPick of draftId: DraftId' * draftPick: DraftPick' * userId: UserId'
    | ContestedPick of
        draftId: DraftId' *
        draftPick: DraftPick' *
        userDetails: (UserId' * uint32 * float option) list *
        winner: UserId'
    | PickPriorityChanged of draftId: DraftId' * userId: UserId' * pickPriority: uint32
    | Picked of
        draftId: DraftId' *
        draftOrdinal: DraftOrdinal *
        draftPick: DraftPick' *
        userId: UserId' *
        timestamp: DateTimeOffset
    | DraftProcessed of draftId: DraftId'
    | DraftFreeSelection of draftId: DraftId'
    | FreePick of draftId: DraftId' * draftPick: DraftPick' * userId: UserId' * timestamp: DateTimeOffset

type Draft' = { DraftStatus: DraftStatus' }

type DraftHelper'() =
    let rec applyEvents events draftsAndRvn =
        match draftsAndRvn, events with
        | None, DraftCreated(_, draftOrdinal, draftType) :: t ->
            applyEvents
                t
                (Some(
                    {
                        DraftStatus =
                            match draftType with
                            | DraftType'.Constrained(starts, ends) ->
                                DraftStatus'.ConstrainedDraft(
                                    draftOrdinal,
                                    ConstrainedStatus'.PendingOpen(starts, ends)
                                )
                            | DraftType'.Unconstrained ->
                                DraftStatus'.UnconstrainedDraft UnconstrainedStatus'.PendingFreeSelection
                    },
                    Rvn.InitialRvn
                ))
        | Some(draft, rvn), DraftOpened _ :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal, ConstrainedStatus'.PendingOpen(starts, ends)) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(draftOrdinal, ConstrainedStatus'.Opened ends)
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof DraftOpened} when not {nameof ConstrainedStatus'.PendingOpen}"
        | Some(draft, rvn), DraftPendingProcessing _ :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal, ConstrainedStatus'.Opened _) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(draftOrdinal, ConstrainedStatus'.PendingProcessing)
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof DraftPendingProcessing} when not {nameof ConstrainedStatus'.Opened}"
        | Some(draft, rvn), ProcessingStarted(_, seed) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal, ConstrainedStatus'.PendingProcessing) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processing(
                                            [],
                                            [ ProcessingEvent'.ProcessingStarted seed ],
                                            Dictionary<UserId', uint32>()
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof ProcessingStarted} when not {nameof ConstrainedStatus'.PendingProcessing}"
        | Some(draft, rvn), WithdrawnPlayersIgnored(_, ignored) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processing(draftPicks, processingEvents, pickPriorities)) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processing(
                                            draftPicks,
                                            ProcessingEvent'.WithdrawnPlayersIgnored ignored :: processingEvents,
                                            pickPriorities
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof WithdrawnPlayersIgnored} when not {nameof ConstrainedStatus'.Processing}"
        | Some(draft, rvn), RoundStarted(_, round) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processing(draftPicks, processingEvents, pickPriorities)) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processing(
                                            draftPicks,
                                            ProcessingEvent'.RoundStarted round :: processingEvents,
                                            pickPriorities
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof RoundStarted} when not {nameof ConstrainedStatus'.Processing}"
        | Some(draft, rvn), AlreadyPickedIgnored(_, ignored) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processing(draftPicks, processingEvents, pickPriorities)) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processing(
                                            draftPicks,
                                            ProcessingEvent'.AlreadyPickedIgnored ignored :: processingEvents,
                                            pickPriorities
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof AlreadyPickedIgnored} when not {nameof ConstrainedStatus'.Processing}"
        | Some(draft, rvn), NoLongerRequiredIgnored(_, ignored) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processing(draftPicks, processingEvents, pickPriorities)) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processing(
                                            draftPicks,
                                            ProcessingEvent'.NoLongerRequiredIgnored ignored :: processingEvents,
                                            pickPriorities
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof NoLongerRequiredIgnored} when not {nameof ConstrainedStatus'.Processing}"
        | Some(draft, rvn), UncontestedPick(_, draftPick, userId) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processing(draftPicks, processingEvents, pickPriorities)) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processing(
                                            draftPicks,
                                            ProcessingEvent'.UncontestedPick(draftPick, userId) :: processingEvents,
                                            pickPriorities
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof UncontestedPick} when not {nameof ConstrainedStatus'.Processing}"
        | Some(draft, rvn), ContestedPick(_, draftPick, userDetails, winner) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processing(draftPicks, processingEvents, pickPriorities)) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processing(
                                            draftPicks,
                                            ProcessingEvent'.ContestedPick(draftPick, userDetails, winner)
                                            :: processingEvents,
                                            pickPriorities
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof ContestedPick} when not {nameof ConstrainedStatus'.Processing}"
        | Some(draft, rvn), PickPriorityChanged(_, userId, pickPriority) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processing(draftPicks, processingEvents, pickPriorities)) ->
                if pickPriorities.ContainsKey userId then
                    pickPriorities[userId] <- pickPriority
                else
                    pickPriorities.Add(userId, pickPriority)

                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processing(
                                            draftPicks,
                                            ProcessingEvent'.PickPriorityChanged(userId, pickPriority)
                                            :: processingEvents,
                                            pickPriorities
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof PickPriorityChanged} when not {nameof ConstrainedStatus'.Processing}"
        | Some(draft, rvn), Picked(_, draftOrdinal, draftPick, userId, timestamp) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processing(draftPicks, processingEvents, pickPriorities)) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processing(
                                            (draftPick, (userId, Some draftOrdinal, timestamp)) :: draftPicks,
                                            ProcessingEvent'.Picked(draftOrdinal, draftPick, userId, timestamp)
                                            :: processingEvents,
                                            pickPriorities
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error $"Invalid {nameof DraftEvent'}: {nameof Picked} when not {nameof ConstrainedStatus'.Processing}"
        | Some(draft, rvn), DraftProcessed _ :: t ->
            match draft.DraftStatus with
            | DraftStatus'.ConstrainedDraft(draftOrdinal,
                                            ConstrainedStatus'.Processing(draftPicks, processingEvents, pickPriorities)) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.ConstrainedDraft(
                                        draftOrdinal,
                                        ConstrainedStatus'.Processed(
                                            draftPicks |> List.rev,
                                            processingEvents |> List.rev,
                                            pickPriorities
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof DraftProcessed} when not {nameof ConstrainedStatus'.Processing}"
        | Some(draft, rvn), DraftFreeSelection _ :: t ->
            match draft.DraftStatus with
            | DraftStatus'.UnconstrainedDraft UnconstrainedStatus'.PendingFreeSelection ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus = DraftStatus'.UnconstrainedDraft(UnconstrainedStatus'.FreeSelection [])
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof DraftFreeSelection} when not {nameof UnconstrainedStatus'.PendingFreeSelection}"
        | Some(draft, rvn), FreePick(_, draftPick, userId, timestamp) :: t ->
            match draft.DraftStatus with
            | DraftStatus'.UnconstrainedDraft(UnconstrainedStatus'.FreeSelection draftPicks) ->
                applyEvents
                    t
                    (Some(
                        {
                            draft with
                                DraftStatus =
                                    DraftStatus'.UnconstrainedDraft(
                                        UnconstrainedStatus'.FreeSelection(
                                            (draftPick, (userId, None, timestamp)) :: (draftPicks |> List.rev)
                                            |> List.rev
                                        )
                                    )
                        },
                        rvn.NextRvn
                    ))
            | _ ->
                Error
                    $"Invalid {nameof DraftEvent'}: {nameof FreePick} when not {nameof UnconstrainedStatus'.FreeSelection}"
        | Some draftAndRvn, [] -> Ok draftAndRvn
        | None, [] -> Error $"No initial {nameof DraftEvent'}"
        | None, h :: _ -> Error $"Invalid initial {nameof DraftEvent'}: {h}"
        | Some _, DraftCreated _ :: _ -> Error $"Invalid non-initial {nameof DraftEvent'}: {nameof DraftCreated}"

    interface IHelper<DraftEvent', Draft'> with
        member _.ApplyEvents events = applyEvents events None
