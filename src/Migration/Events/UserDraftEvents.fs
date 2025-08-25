namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open System.Collections.Generic

type UserDraftEvent' =
    | UserDraftCreated of userDraftId: UserDraftId' * userId: UserId' * draftId: DraftId'
    | Drafted of userDraftId: UserDraftId' * userDraftPick: UserDraftPick'
    | Undrafted of userDraftId: UserDraftId' * userDraftPick: UserDraftPick'
    | PriorityChanged of userDraftId: UserDraftId' * userDraftPick: UserDraftPick' * priorityChange: PriorityChange

type UserDraft' = {
    UserDraftKey: UserId' * DraftId'
    UserDraftPicks: Dictionary<UserDraftPick', int>
}

type UserDraftHelper'() =
    let rec applyEvents events userDraftsAndRvn =
        match userDraftsAndRvn, events with
        | None, UserDraftCreated(_, userId, draftId) :: t ->
            applyEvents
                t
                (Some(
                    {
                        UserDraftKey = userId, draftId
                        UserDraftPicks = Dictionary<UserDraftPick', int>()
                    },
                    Rvn.InitialRvn
                ))
        | Some(userDraft, rvn), Drafted(_, userDraftPick) :: t ->
            if not (userDraft.UserDraftPicks.ContainsKey userDraftPick) then
                userDraft.UserDraftPicks.Add(userDraftPick, userDraft.UserDraftPicks.Count + 1)

                applyEvents t (Some(userDraft, rvn.NextRvn))
            else
                Error $"Invalid {nameof UserDraftEvent'}: {nameof Drafted} when already drafted"
        | Some(userDraft, rvn), Undrafted(_, userDraftPick) :: t ->
            if userDraft.UserDraftPicks.ContainsKey userDraftPick then
                let userDraftPicks = Dictionary<UserDraftPick', int>()
                userDraft.UserDraftPicks.Remove userDraftPick |> ignore

                userDraft.UserDraftPicks
                |> List.ofSeq
                |> List.map (fun (KeyValue(userDraftPick, rank)) -> userDraftPick, rank)
                |> List.sortBy snd
                |> List.iteri (fun i (userDraftPick, _) -> userDraftPicks.Add(userDraftPick, i + 1))

                applyEvents
                    t
                    (Some(
                        {
                            userDraft with
                                UserDraftPicks = userDraftPicks
                        },
                        rvn.NextRvn
                    ))
            else
                Error $"Invalid {nameof UserDraftEvent'}: {nameof Undrafted} when not drafted"
        | Some(userDraft, rvn), PriorityChanged(_, userDraftPick, priorityChange) :: t ->
            if userDraft.UserDraftPicks.ContainsKey userDraftPick then
                let userDraftPicks = Dictionary<UserDraftPick', int>()

                let adjustment =
                    match priorityChange with
                    | Increase -> -1.5
                    | Decrease -> 1.5

                userDraft.UserDraftPicks
                |> List.ofSeq
                |> List.map (fun (KeyValue(existingUserDraftPick, rank)) ->
                    let adjustedRank =
                        if existingUserDraftPick = userDraftPick then
                            float rank + adjustment
                        else
                            float rank

                    existingUserDraftPick, adjustedRank)
                |> List.sortBy snd
                |> List.iteri (fun i (userDraftPick, _) -> userDraftPicks.Add(userDraftPick, i + 1))

                applyEvents
                    t
                    (Some(
                        {
                            userDraft with
                                UserDraftPicks = userDraftPicks
                        },
                        rvn.NextRvn
                    ))
            else
                Error $"Invalid {nameof UserDraftEvent'}: {nameof PriorityChanged} when not drafted"
        | Some userDraftsAndRvn, [] -> Ok userDraftsAndRvn
        | None, [] -> Error $"No initial {nameof UserDraftEvent'}"
        | None, h :: _ -> Error $"Invalid initial {nameof UserDraftEvent'}: {h}"
        | Some _, UserDraftCreated _ :: _ ->
            Error $"Invalid non-initial {nameof UserDraftEvent'}: {nameof UserDraftCreated}"

    interface IHelper<UserDraftEvent', UserDraft'> with
        member _.ApplyEvents events = applyEvents events None
