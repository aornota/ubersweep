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
    | DraftOpened of ends: DateTimeOffset
    | DraftClosed
    | ProcessingStarted
    (* TODO-ENTITIES: These are really ProcessingEvents...
    | WithdrawnPlayersIgnored of ignored: (UserId * (SquadId * PlayerId) list) list
    | RoundStarted of round: uint32
    | AlreadyPickedIgnored of ignored: (UserId * DraftPick list) list
    | NoLongerRequiredIgnored of ignored: (UserId * DraftPick list) list
    | UncontestedPick of draftPick: DraftPick * userId: UserId
    | ContestedPick of draftPick: DraftPick * userDetails: (UserId * uint32 * float option) list * winner: UserId
    | PickPriorityChanged of userId: UserId * pickPriority: uint32
    | Picked of draftOrdinal: DraftOrdinal * draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset
    *)
    | ProcessingFinished of
        draftPicks: (DraftPick * PickedBy) list *
        processingEvents: ProcessingEvent list *
        pickPriorities: Map<UserId, uint32>
    | FreeSelectionOpened
    | FreePickMade of draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset

    interface IEvent with
        member this.EventJson = Json.toJson this

type Draft = {
    DraftCommon: DraftCommon'
} with

    interface IState<Draft, DraftEvent> with
        member this.SnapshotJson = Json.toJson this

        member this.Evolve event =
            match event with
            | DraftOpened ends ->
                Ok {
                    this with
                        DraftCommon.DraftStatus = Opened ends
                }
            | DraftClosed ->
                Ok {
                    this with
                        DraftCommon.DraftStatus = PendingProcessing
                }
            | ProcessingStarted ->
                Ok {
                    this with
                        DraftCommon.DraftStatus = Processing
                }
            | ProcessingFinished(draftPicks, processingEvents, pickPriorities) ->
                Ok {
                    this with
                        DraftCommon.DraftStatus = Processing
                        DraftCommon.DraftPicks = draftPicks
                        DraftCommon.ProcessingEvents = processingEvents
                        DraftCommon.PickPriorities = pickPriorities
                }
            | FreeSelectionOpened ->
                Ok {
                    this with
                        DraftCommon.DraftStatus = FreeSelection
                }
            | FreePickMade(draftPick, userId, timestamp) ->
                Ok {
                    this with
                        DraftCommon.DraftPicks =
                            (draftPick, (userId, None, timestamp))
                            :: (this.DraftCommon.DraftPicks |> List.rev)
                            |> List.rev
                }

type DraftHelper() =
    inherit EntityHelper<DraftId, Draft, DraftInitCommand, DraftInitEvent, DraftEvent>()

    override _.IdFromGuid guid = DraftId.FromGuid guid

    override _.InitFromCommand(guid, CreateDraft(draftOrdinal, draftType)) =
        {
            Id = DraftId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = {
                DraftCommon = {
                    DraftOrdinal = draftOrdinal
                    DraftStatus =
                        match draftType with
                        | Constrained(starts, ends) -> PendingOpen(starts, ends)
                        | Unconstrained -> PendingFreeSelection
                    DraftPicks = []
                    ProcessingEvents = []
                    PickPriorities = Map.empty<UserId, uint32>
                }
            }
        },
        DraftCreated(draftOrdinal, draftType)

    override _.InitFromEvent(guid, DraftCreated(draftOrdinal, draftType)) = {
        Id = DraftId.FromGuid guid
        Rvn = Rvn.InitialRvn
        State = {
            DraftCommon = {
                DraftOrdinal = draftOrdinal
                DraftStatus =
                    match draftType with
                    | Constrained(starts, ends) -> PendingOpen(starts, ends)
                    | Unconstrained -> PendingFreeSelection
                DraftPicks = []
                ProcessingEvents = []
                PickPriorities = Map.empty<UserId, uint32>
            }
        }
    }

[<RequireQualifiedAccess>]
module Draft =
    let private decide (command: DraftCommand) (draft: Draft) =
        match command with
        | OpenDraft ->
            match draft.DraftCommon.DraftStatus with
            | PendingOpen(_, ends) -> Ok(DraftOpened ends)
            | _ -> Error $"{nameof OpenDraft} when {nameof Draft} is not {nameof PendingOpen}"
        | CloseDraft ->
            match draft.DraftCommon.DraftStatus with
            | Opened _ -> Ok DraftClosed
            | _ -> Error $"{nameof CloseDraft} when {nameof Draft} is not {nameof Opened}"
        | StartProcessing ->
            match draft.DraftCommon.DraftStatus with
            | PendingProcessing -> Ok ProcessingStarted
            | _ -> Error $"{nameof StartProcessing} when {nameof Draft} is not {nameof PendingProcessing}"
        | FinishProcesing(draftPicks, processingEvents, pickPriorities) ->
            match draft.DraftCommon.DraftStatus with
            | Processing -> Ok(ProcessingFinished(draftPicks, processingEvents, pickPriorities))
            | _ -> Error $"{nameof FinishProcesing} when {nameof Draft} is not {nameof Processing}"
        | OpenFreeSelection ->
            match draft.DraftCommon.DraftStatus with
            | PendingFreeSelection -> Ok FreeSelectionOpened
            | _ -> Error $"{nameof OpenFreeSelection} when {nameof Draft} is not {nameof PendingFreeSelection}"
        | MakeFreePick(draftPick, userId, timestamp) ->
            match draft.DraftCommon.DraftStatus with
            | FreeSelection -> Ok(FreePickMade(draftPick, userId, timestamp))
            | _ -> Error $"{nameof MakeFreePick} when {nameof Draft} is not {nameof FreeSelection}"

    let helper = DraftHelper()

    let apply command (entity: Entity<DraftId, Draft, DraftEvent>) = result {
        let! event = decide command entity.State
        let! entity = entity.Evolve event
        return entity, event
    }
