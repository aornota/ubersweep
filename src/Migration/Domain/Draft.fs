namespace Aornota.Ubersweep.Migration.Domain

open Aornota.Ubersweep.Shared.Common

open System

type DraftId =
    | DraftId of guid: Guid

    static member Create() = Guid.NewGuid() |> DraftId

type DraftType =
    | Constrained of starts: DateTimeOffset * ends: DateTimeOffset
    | Unconstrained

type DraftStatus =
    | PendingOpen of starts: DateTimeOffset * ends: DateTimeOffset
    | Opened of ends: DateTimeOffset
    | PendingProcessing of processingStarted: bool
    | Processed
    | PendingFreeSelection
    | FreeSelection

type DraftPick =
    | TeamPicked of squadId: SquadId
    | PlayerPicked of squadId: SquadId * playerId: PlayerId

type UserDraftPick =
    | TeamPick of squadId: SquadId
    | PlayerPick of squadId: SquadId * playerId: PlayerId

type UserDraftPickDto = {
    UserDraftPick: UserDraftPick
    Rank: int
}

type ProcessingEvent =
    | ProcessingStarted of seed: int
    | WithdrawnPlayersIgnored of ignored: (UserId * (SquadId * PlayerId) list) list
    | RoundStarted of round: uint32
    | AlreadyPickedIgnored of ignored: (UserId * DraftPick list) list
    | NoLongerRequiredIgnored of ignored: (UserId * DraftPick list) list
    | UncontestedPick of draftPick: DraftPick * userId: UserId
    | ContestedPick of draftPick: DraftPick * userDetails: (UserId * uint32 * float option) list * winner: UserId
    | PickPriorityChanged of userId: UserId * pickPriority: uint32
    | Picked of draftOrdinal: DraftOrdinal * draftPick: DraftPick * userId: UserId * timestamp: DateTimeOffset

type ProcessingDetails = {
    UserDraftPicks: (UserId * UserDraftPickDto list) list
    ProcessingEvents: ProcessingEvent list
}

type DraftDto = {
    DraftId: DraftId
    Rvn: Rvn
    DraftOrdinal: DraftOrdinal
    DraftStatus: DraftStatus
    ProcessingDetails: ProcessingDetails option
}

type PriorityChange =
    | Increase
    | Decrease

type UserDraftKey = UserId * DraftId

type CurrentUserDraftDto = {
    UserDraftKey: UserDraftKey
    Rvn: Rvn
    UserDraftPickDtos: UserDraftPickDto list
}

type UserDraftSummaryDto = {
    UserDraftKey: UserDraftKey
    PickCount: int
}
