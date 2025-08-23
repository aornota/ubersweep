namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System

type UserDraftOnitEvent =
    | UserDraftCreated of userId: UserId * draftId: DraftId

    interface IEvent with
        member this.EventJson = Json.toJson this

type UserDraftEvent =
    | Drafted of userDraftPick: UserDraftPick
    | Undrafted of userDraftPick: UserDraftPick
    | PriorityChanged of userDraftPick: UserDraftPick * priorityChange: PriorityChange

    interface IEvent with
        member this.EventJson = Json.toJson this

type UserDraft = { // TODO-ENTITIES: Implement this properly...
    Dummy: unit
} with

    interface IState<UserDraft, UserDraftEvent> with
        member this.SnapshotJson = Json.toJson this

        member this.Evolve event = Ok this
