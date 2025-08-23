namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type SquadId =
    private
    | SquadId of guid: Guid

    static member Create() = SquadId(Guid.NewGuid())
    static member FromGuid guid = SquadId guid

    interface IId with
        member this.Guid =
            let (SquadId guid) = this
            guid

type Seeding = Seeding of seeding: uint32

type PlayerId =
    private
    | PlayerId of guid: Guid

    static member Create() = PlayerId(Guid.NewGuid())
    static member FromGuid guid = PlayerId guid

    interface IId with
        member this.Guid =
            let (PlayerId guid) = this
            guid

type PlayerTypeFootball =
    | Goalkeeper
    | Defender
    | Midfielder
    | Forward

type PlayerTypeRugby =
    | Forward
    | Back

type PlayerStatus =
    | Active
    | Withdrawn of dateWithdrawn: DateTimeOffset option

type SquadInitCommand<'group> =
    | CreateSquad of squadName: string * group: 'group * seeding: Seeding option * coachName: string

type SquadCommand<'playerType> =
    | AddPlayer of playerId: PlayerId * playerName: string * playerType: 'playerType
    | ChangePlayerName of playerId: PlayerId * playerName: string
    | ChangePlayerType of playerId: PlayerId * playerType: 'playerType
    | WithdrawPlayer of playerId: PlayerId * dateWithdrawn: DateTimeOffset option
    | EliminateSquad

type Player<'playerType> = {
    PlayerName: string
    PlayerType: 'playerType
    PlayerStatus: PlayerStatus
}

type SquadCommon'<'group, 'playerType> = {
    SquadName: string
    Group: 'group
    Seeding: Seeding option
    CoachName: string
    Eliminated: bool
    Players: Map<PlayerId, Player<'playerType>>
}

type PickedBy = UserId * DraftOrdinal option * DateTimeOffset
