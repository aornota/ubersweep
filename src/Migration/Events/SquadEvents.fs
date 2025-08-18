namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Domain

open System

type SquadEvent =
    | SquadCreated of
        squadId: SquadId *
        squadName: SquadName *
        group: IGroup *
        seeding: Seeding option *
        coachName: CoachName // TODO: Cannot deserialize to interface?...
    | PlayerAdded of squadId: SquadId * playerId: PlayerId * playerName: PlayerName * playerType: IPlayerType // TODO: Cannot deserialize to interface?...
    | PlayerNameChanged of squadId: SquadId * playerId: PlayerId * playerName: PlayerName
    | PlayerTypeChanged of squadId: SquadId * playerId: PlayerId * playerType: IPlayerType // TODO: Cannot deserialize to interface?...
    | PlayerWithdrawn of squadId: SquadId * playerId: PlayerId * dateWithdrawn: DateTimeOffset option
    | SquadEliminated of squadId: SquadId
