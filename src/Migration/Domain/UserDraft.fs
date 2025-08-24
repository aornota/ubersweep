namespace Aornota.Ubersweep.Migration.Domain

open System

type UserDraftId =
    | UserDraftId of guid: Guid

    static member Create() = Guid.NewGuid() |> UserDraftId

type UserDraftPick =
    | TeamPick of squadId: SquadId
    | PlayerPick of squadId: SquadId * playerId: PlayerId
