namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open Thoth.Json.Net

type UserDraftInitEvent =
    | UserDraftCreated of userId: UserId * draftId: DraftId

    interface IEvent with
        member this.EventJson = Json.encode this

type UserDraftEvent =
    | DraftPickAdded of userDraftPick: UserDraftPick
    | DraftPickRemoved of userDraftPick: UserDraftPick
    | DraftPickPriorityChanged of userDraftPick: UserDraftPick * priorityChange: PriorityChange

    interface IEvent with
        member this.EventJson = Json.encode this

type UserDraft = {
    UserDraftCommon: UserDraftCommon'
} with

    interface IState<UserDraft, UserDraftEvent> with
        member this.SnapshotJson = Json.encode this

        member this.Evolve event =
            match event with
            | DraftPickAdded userDraftPick ->
                match this.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
                | Some _ -> Error $"{nameof DraftPickAdded} when {userDraftPick} already in {nameof UserDraft}"
                | None ->
                    Ok {
                        this with
                            UserDraftCommon.UserDraftPicks =
                                this.UserDraftCommon.UserDraftPicks
                                |> Map.add userDraftPick (this.UserDraftCommon.UserDraftPicks.Count + 1)
                    }
            | DraftPickRemoved userDraftPick ->
                match this.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
                | Some _ ->
                    let list =
                        this.UserDraftCommon.UserDraftPicks
                        |> Map.remove userDraftPick
                        |> List.ofSeq
                        |> List.mapi (fun i kvp -> kvp.Key, i + 1)

                    Ok {
                        this with
                            UserDraftCommon.UserDraftPicks = list |> Map.ofList
                    }
                | None -> Error $"{nameof DraftPickRemoved} when {userDraftPick} not in {nameof UserDraft}"
            | DraftPickPriorityChanged(userDraftPick, priorityChange) ->
                match this.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
                | Some _ ->
                    let adjustment =
                        match priorityChange with
                        | Increase -> -1.5
                        | Decrease -> 1.5

                    let list =
                        this.UserDraftCommon.UserDraftPicks
                        |> List.ofSeq
                        |> List.map (fun kvp ->
                            let adjustedRank =
                                if kvp.Key = userDraftPick then
                                    float kvp.Value + adjustment
                                else
                                    float kvp.Value

                            kvp.Key, adjustedRank)
                        |> List.sortBy snd
                        |> List.mapi (fun i (userDraftPick, _) -> userDraftPick, i + 1)

                    Ok {
                        this with
                            UserDraftCommon.UserDraftPicks = list |> Map.ofList
                    }
                | None -> Error $"{nameof DraftPickRemoved} when {userDraftPick} not in {nameof UserDraft}"

type UserDraftelper() =
    inherit EntityHelper<UserDraftId, UserDraft, UserDraftInitCommand, UserDraftInitEvent, UserDraftEvent>()

    let eventDecoder =
        Decode.Auto.generateDecoderCached<UserDraftEvent> (Json.caseStrategy, Json.extraCoders)

    let initEventDecoder =
        Decode.Auto.generateDecoderCached<UserDraftInitEvent> (Json.caseStrategy, Json.extraCoders)

    let stateDecoder =
        Decode.Auto.generateDecoderCached<UserDraft> (Json.caseStrategy, Json.extraCoders)

    override _.DecodeEvent(Json json) = Decode.fromString eventDecoder json
    override _.DecodeInitEvent(Json json) = Decode.fromString initEventDecoder json
    override _.DecodeState(Json json) = Decode.fromString stateDecoder json
    override _.IdFromGuid guid = UserDraftId.FromGuid guid

    override _.InitFromCommand(guid, CreateUserDraft(userId, draftId)) =
        {
            Id = UserDraftId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = {
                UserDraftCommon = {
                    UserDraftKey = userId, draftId
                    UserDraftPicks = Map.empty<UserDraftPick, int>
                }
            }
        },
        UserDraftCreated(userId, draftId)

    override _.InitFromEvent(guid, UserDraftCreated(userId, draftId)) = {
        Id = UserDraftId.FromGuid guid
        Rvn = Rvn.InitialRvn
        State = {
            UserDraftCommon = {
                UserDraftKey = userId, draftId
                UserDraftPicks = Map.empty<UserDraftPick, int>
            }
        }
    }

[<RequireQualifiedAccess>]
module UserDraft =
    let private decide (command: UserDraftCommand) (userDraft: UserDraft) =
        match command with
        | AddDraftPick userDraftPick ->
            match userDraft.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
            | Some _ -> Error $"{nameof AddDraftPick} when {userDraftPick} already in {nameof UserDraft}"
            | None -> Ok(DraftPickAdded userDraftPick)
        | RemoveDraftPick userDraftPick ->
            match userDraft.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
            | Some _ -> Ok(DraftPickRemoved userDraftPick)
            | None -> Error $"{nameof RemoveDraftPick} when {userDraftPick} not in {nameof UserDraft}"
        | ChangeDraftPickPriority(userDraftPick, priorityChange) ->
            match userDraft.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
            | Some _ -> Ok(DraftPickPriorityChanged(userDraftPick, priorityChange))
            | None -> Error $"{nameof ChangeDraftPickPriority} when {userDraftPick} not in {nameof UserDraft}"

    let helper = UserDraftelper()

    let apply command (entity: Entity<UserDraftId, UserDraft, UserDraftEvent>) = result {
        let! event = decide command entity.State
        let! entity = entity.Evolve event
        return entity, event
    }
