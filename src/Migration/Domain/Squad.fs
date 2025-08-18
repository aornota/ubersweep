namespace Aornota.Ubersweep.Migration.Domain

open Aornota.Ubersweep.Shared.Common

open System

type IPlayerType = interface end

type SquadId =
    | SquadId of guid: Guid

    static member Create() = Guid.NewGuid() |> SquadId

type SquadName = SquadName of squadName: string
type CoachName = CoachName of coachName: string

type Seeding = Seeding of seeding: int

type PlayerId =
    | PlayerId of guid: Guid

    static member Create() = Guid.NewGuid() |> PlayerId

type PlayerName = PlayerName of playerName: string

type PlayerTypeFootball =
    | Goalkeeper
    | Defender
    | Midfielder
    | Forward

    interface IPlayerType

type PlayerTypeRugby =
    | Forward
    | Back

    interface IPlayerType

type PlayerStatus =
    | Active
    | Withdrawn of dateWithdrawn: DateTimeOffset option

type PickedBy = UserId * DraftOrdinal option * DateTimeOffset

type PlayerDto = {
    PlayerId: PlayerId
    PlayerName: PlayerName
    PlayerType: IPlayerType
    PlayerStatus: PlayerStatus
    PickedBy: PickedBy option
}

type SquadOnlyDto = {
    SquadId: SquadId
    Rvn: Rvn
    SquadName: SquadName
    Group: IGroup
    Seeding: Seeding option
    CoachName: CoachName
    Eliminated: bool
    PickedBy: PickedBy option
}

type SquadDto = {
    SquadOnlyDto: SquadOnlyDto
    PlayerDtos: PlayerDto list
}
