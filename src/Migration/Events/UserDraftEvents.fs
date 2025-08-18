namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common

type UserDraftEvent =
    | UserDraftCreated of userDraftId: UserDraftId * userId: UserId * draftId: DraftId
    | Drafted of userDraftId: UserDraftId * userDraftPick: UserDraftPick
    | Undrafted of userDraftId: UserDraftId * userDraftPick: UserDraftPick
    | PriorityChanged of userDraftId: UserDraftId * userDraftPick: UserDraftPick * priorityChange: PriorityChange
