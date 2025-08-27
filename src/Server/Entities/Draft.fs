namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System
open Thoth.Json.Net

type DraftInitEvent =
    | DraftCreated of draftType: DraftType

    interface IEvent with
        member this.EventJson = Json.encode this

type DraftEvent =
    | DraftOpened
    | DraftClosed
    | ProcessingStarted
    | ProcessingFinished of
        draftPicks: (DraftPick * PickedBy) list *
        processingEvents: ProcessingEvent list *
        pickPriorities: Map<UserId, uint32>
    | FreeSelectionOpened
    | FreePickMade of draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset

    interface IEvent with
        member this.EventJson = Json.encode this

type Draft = {
    DraftCommon: DraftCommon'
} with

    interface IState<Draft, DraftEvent> with
        member this.SnapshotJson = Json.encode this

        member this.Evolve event =
            match event with
            | DraftOpened ->
                match this.DraftCommon.DraftStatus with
                | ConstrainedDraft(draftOrdinal, PendingOpen(_, ends)) ->
                    Ok {
                        this with
                            DraftCommon.DraftStatus = ConstrainedDraft(draftOrdinal, Opened ends)
                    }
                | _ -> Error $"{nameof DraftOpened} when {nameof Draft} not {nameof PendingOpen}"
            | DraftClosed ->
                match this.DraftCommon.DraftStatus with
                | ConstrainedDraft(draftOrdinal, Opened _) ->
                    Ok {
                        this with
                            DraftCommon.DraftStatus = ConstrainedDraft(draftOrdinal, PendingProcessing)
                    }
                | _ -> Error $"{nameof DraftClosed} when {nameof Draft} not {nameof Opened}"
            | ProcessingStarted ->
                match this.DraftCommon.DraftStatus with
                | ConstrainedDraft(draftOrdinal, PendingProcessing) ->
                    Ok {
                        this with
                            DraftCommon.DraftStatus = ConstrainedDraft(draftOrdinal, Processing)
                    }
                | _ -> Error $"{nameof ProcessingStarted} when {nameof Draft} not {nameof PendingProcessing}"
            | ProcessingFinished(draftPicks, processingEvents, pickPriorities) ->
                match this.DraftCommon.DraftStatus with
                | ConstrainedDraft(draftOrdinal, Processing) ->
                    Ok {
                        this with
                            DraftCommon.DraftStatus =
                                ConstrainedDraft(draftOrdinal, Processed(draftPicks, processingEvents, pickPriorities))
                    }
                | _ -> Error $"{nameof ProcessingFinished} when {nameof Draft} not {nameof Processing}"
            | FreeSelectionOpened ->
                match this.DraftCommon.DraftStatus with
                | UnconstrainedDraft PendingFreeSelection ->
                    Ok {
                        this with
                            DraftCommon.DraftStatus = UnconstrainedDraft(FreeSelection [])
                    }
                | _ -> Error $"{nameof FreeSelectionOpened} when {nameof Draft} not {nameof PendingFreeSelection}"
            | FreePickMade(draftPick, userId, timestamp) ->
                match this.DraftCommon.DraftStatus with
                | UnconstrainedDraft(FreeSelection draftPicks) ->
                    Ok {
                        this with
                            DraftCommon.DraftStatus =
                                UnconstrainedDraft(
                                    FreeSelection(
                                        (draftPick, (userId, None, timestamp)) :: (draftPicks |> List.rev) |> List.rev
                                    )
                                )
                    }
                | _ -> Error $"{nameof FreePickMade} when {nameof Draft} not {nameof FreeSelection}"

type DraftHelper() =
    inherit EntityHelper<DraftId, Draft, DraftInitCommand, DraftInitEvent, DraftEvent>()

    let eventDecoder =
        Decode.Auto.generateDecoderCached<DraftEvent> (Json.caseStrategy, Json.extraCoders)

    let initEventDecoder =
        Decode.Auto.generateDecoderCached<DraftInitEvent> (Json.caseStrategy, Json.extraCoders)

    let stateDecoder =
        Decode.Auto.generateDecoderCached<Draft> (Json.caseStrategy, Json.extraCoders)

    override _.DecodeEvent(Json json) = Decode.fromString eventDecoder json
    override _.DecodeInitEvent(Json json) = Decode.fromString initEventDecoder json
    override _.DecodeState(Json json) = Decode.fromString stateDecoder json

    override _.IdFromGuid guid = DraftId.FromGuid guid

    override _.InitFromCommand(guid, CreateDraft draftType) =
        {
            Id = DraftId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = {
                DraftCommon = {
                    DraftStatus =
                        match draftType with
                        | Constrained(draftOrdinal, starts, ends) ->
                            ConstrainedDraft(draftOrdinal, PendingOpen(starts, ends))
                        | Unconstrained -> UnconstrainedDraft PendingFreeSelection
                }
            }
        },
        DraftCreated draftType

    override _.InitFromEvent(guid, DraftCreated draftType) = {
        Id = DraftId.FromGuid guid
        Rvn = Rvn.InitialRvn
        State = {
            DraftCommon = {
                DraftStatus =
                    match draftType with
                    | Constrained(draftOrdinal, starts, ends) ->
                        ConstrainedDraft(draftOrdinal, PendingOpen(starts, ends))
                    | Unconstrained -> UnconstrainedDraft PendingFreeSelection
            }
        }
    }

[<RequireQualifiedAccess>]
module Draft =
    let private decide (command: DraftCommand) (draft: Draft) =
        match command with
        | OpenDraft ->
            match draft.DraftCommon.DraftStatus with
            | ConstrainedDraft(_, PendingOpen(_, ends)) -> Ok DraftOpened
            | _ -> Error $"{nameof OpenDraft} when {nameof Draft} is not {nameof PendingOpen}"
        | CloseDraft ->
            match draft.DraftCommon.DraftStatus with
            | ConstrainedDraft(_, Opened _) -> Ok DraftClosed
            | _ -> Error $"{nameof CloseDraft} when {nameof Draft} is not {nameof Opened}"
        | StartProcessing ->
            match draft.DraftCommon.DraftStatus with
            | ConstrainedDraft(_, PendingProcessing) -> Ok ProcessingStarted
            | _ -> Error $"{nameof StartProcessing} when {nameof Draft} is not {nameof PendingProcessing}"
        | FinishProcesing(draftPicks, processingEvents, pickPriorities) ->
            match draft.DraftCommon.DraftStatus with
            | ConstrainedDraft(_, Processing) -> Ok(ProcessingFinished(draftPicks, processingEvents, pickPriorities))
            | _ -> Error $"{nameof FinishProcesing} when {nameof Draft} is not {nameof Processing}"
        | OpenFreeSelection ->
            match draft.DraftCommon.DraftStatus with
            | UnconstrainedDraft PendingFreeSelection -> Ok FreeSelectionOpened
            | _ -> Error $"{nameof OpenFreeSelection} when {nameof Draft} is not {nameof PendingFreeSelection}"
        | MakeFreePick(draftPick, userId, timestamp) ->
            match draft.DraftCommon.DraftStatus with
            | UnconstrainedDraft(FreeSelection _) -> Ok(FreePickMade(draftPick, userId, timestamp))
            | _ -> Error $"{nameof MakeFreePick} when {nameof Draft} is not {nameof FreeSelection}"

    let helper = DraftHelper()

    let apply command (entity: Entity<DraftId, Draft, DraftEvent>) = result {
        let! event = decide command entity.State
        let! entity = entity.Evolve event
        return entity, event
    }
