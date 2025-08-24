namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System

type UserDraftInitEvent =
    | UserDraftCreated of userId: UserId * draftId: DraftId

    interface IEvent with
        member this.EventJson = Json.toJson this

type UserDraftEvent =
    | Drafted of userDraftPick: UserDraftPick
    | Undrafted of userDraftPick: UserDraftPick
    | PriorityChanged of userDraftPick: UserDraftPick * priorityChange: PriorityChange

    interface IEvent with
        member this.EventJson = Json.toJson this

type UserDraft = {
    UserDraftCommon: UserDraftCommon'
} with

    interface IState<UserDraft, UserDraftEvent> with
        member this.SnapshotJson = Json.toJson this

        member this.Evolve event =
            match event with
            | Drafted userDraftPick ->
                match this.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
                | Some _ -> Error $"{nameof Drafted} when {userDraftPick} already in {nameof UserDraft}"
                | None ->
                    Ok {
                        this with
                            UserDraftCommon.UserDraftPicks =
                                this.UserDraftCommon.UserDraftPicks
                                |> Map.add userDraftPick (this.UserDraftCommon.UserDraftPicks.Count + 1)
                    }
            | Undrafted userDraftPick ->
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
                | None -> Error $"{nameof Undrafted} when {userDraftPick} not in {nameof UserDraft}"
            | PriorityChanged(userDraftPick, priorityChange) ->
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
                | None -> Error $"{nameof Undrafted} when {userDraftPick} not in {nameof UserDraft}"

type UserDraftelper() =
    inherit EntityHelper<UserDraftId, UserDraft, UserDraftInitCommand, UserDraftInitEvent, UserDraftEvent>()

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
        | Draft userDraftPick ->
            match userDraft.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
            | Some _ -> Error $"{nameof Draft} when {userDraftPick} already in {nameof UserDraft}"
            | None -> Ok(Drafted userDraftPick)
        | Undraft userDraftPick ->
            match userDraft.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
            | Some _ -> Ok(Undrafted userDraftPick)
            | None -> Error $"{nameof Undraft} when {userDraftPick} not in {nameof UserDraft}"
        | ChangePriority(userDraftPick, priorityChange) ->
            match userDraft.UserDraftCommon.UserDraftPicks |> Map.tryFind userDraftPick with
            | Some _ -> Ok(PriorityChanged(userDraftPick, priorityChange))
            | None -> Error $"{nameof ChangePriority} when {userDraftPick} not in {nameof UserDraft}"

    let helper = UserDraftelper()

    let apply command (entity: Entity<UserDraftId, UserDraft, UserDraftEvent>) = result {
        let! event = decide command entity.State
        let! entity = entity.Evolve event
        return entity, event
    }
