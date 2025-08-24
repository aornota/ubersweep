namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type UserDraftId =
    private
    | UserDraftId of guid: Guid

    static member Create() = UserDraftId(Guid.NewGuid())
    static member FromGuid guid = UserDraftId guid

    interface IId with
        member this.Guid =
            let (UserDraftId guid) = this
            guid

type UserDraftPick =
    | TeamPick of squadId: SquadId
    | PlayerPick of squadId: SquadId * playerId: PlayerId

type PriorityChange =
    | Increase
    | Decrease

type UserDraftInitCommand = CreateUserDraft of userId: UserId * draftId: DraftId

type UserDraftCommand =
    | AddDraftPick of userDraftPick: UserDraftPick
    | RemoveDraftPick of userDraftPick: UserDraftPick
    | ChangeDraftPickPriority of userDraftPick: UserDraftPick * priorityChange: PriorityChange

type UserDraftCommon' = {
    UserDraftKey: UserId * DraftId
    UserDraftPicks: Map<UserDraftPick, int>
}
