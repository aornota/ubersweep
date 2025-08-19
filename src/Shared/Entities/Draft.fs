namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type DraftId =
    private
    | DraftId of guid: Guid

    static member Create() = DraftId(Guid.NewGuid())
    static member FromGuid guid = DraftId guid

    interface IId with
        member this.Guid =
            let (DraftId guid) = this
            guid

type DraftType =
    | Constrained of starts: DateTimeOffset * ends: DateTimeOffset
    | Unconstrained

type DraftStatus =
    | PendingOpen of starts: DateTimeOffset * ends: DateTimeOffset
    | Opened of ends: DateTimeOffset
    | PendingProcessing
    | Processing
    | Processed
    | PendingFreeSelection
    | FreeSelection

type DraftPick =
    | TeamPicked of squadId: SquadId
    | PlayerPicked of squadId: SquadId * playerId: PlayerId

type UserDraftPick =
    | TeamPick of squadId: SquadId
    | PlayerPick of squadId: SquadId * playerId: PlayerId

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

type PriorityChange =
    | Increase
    | Decrease
