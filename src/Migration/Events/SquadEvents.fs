namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common

open System
open System.Collections.Generic

type SquadName = SquadName of squadName: string
type CoachName = CoachName of coachName: string
type PlayerName = PlayerName of playerName: string

type SquadEvent<'group, 'playerType> =
    | SquadCreated of
        squadId: SquadId *
        squadName: SquadName *
        group: 'group *
        seeding: Seeding option *
        coachName: CoachName
    | PlayerAdded of squadId: SquadId * playerId: PlayerId * playerName: PlayerName * playerType: 'playerType
    | PlayerNameChanged of squadId: SquadId * playerId: PlayerId * playerName: PlayerName
    | PlayerTypeChanged of squadId: SquadId * playerId: PlayerId * playerType: 'playerType
    | PlayerWithdrawn of squadId: SquadId * playerId: PlayerId * dateWithdrawn: DateTimeOffset option
    | SquadEliminated of squadId: SquadId

type Player<'playerType> = {
    PlayerName: string
    PlayerType: 'playerType
    PlayerStatus: PlayerStatus
}

type Squad<'group, 'playerType> = {
    SquadName: string
    Group: 'group
    Seeding: Seeding option
    CoachName: string
    Eliminated: bool
    Players: Dictionary<PlayerId, Player<'playerType>>
}

type SquadHelper<'group, 'playerType>() =
    let rec applyEvents (events: SquadEvent<'group, 'playerType> list) squadAndRvn =
        match squadAndRvn, events with
        | None, SquadCreated(_, SquadName squadName, group, seeding, CoachName coachName) :: t ->
            applyEvents
                t
                (Some(
                    {
                        SquadName = squadName
                        Group = group
                        Seeding = seeding
                        CoachName = coachName
                        Eliminated = false
                        Players = Dictionary<PlayerId, Player<'playerType>>()
                    },
                    Rvn.InitialRvn
                ))
        | Some(squad, rvn), PlayerAdded(_, playerId, PlayerName playerName, playerType) :: t ->
            if not (squad.Players.ContainsKey playerId) then
                squad.Players.Add(
                    playerId,
                    {
                        PlayerName = playerName
                        PlayerType = playerType
                        PlayerStatus = Active
                    }
                )

                applyEvents t (Some(squad, rvn.NextRvn))
            else
                Error $"Invalid {nameof SquadEvent}: {nameof PlayerAdded} when already added"
        | Some(squad, rvn), PlayerNameChanged(_, playerId, PlayerName playerName) :: t ->
            if squad.Players.ContainsKey playerId then
                let player = squad.Players.[playerId]
                squad.Players.[playerId] <- { player with PlayerName = playerName }

                applyEvents t (Some(squad, rvn.NextRvn))
            else
                Error $"Invalid {nameof SquadEvent}: {nameof PlayerNameChanged} when not added"
        | Some(squad, rvn), PlayerTypeChanged(_, playerId, playerType) :: t ->
            if squad.Players.ContainsKey playerId then
                let player = squad.Players.[playerId]
                squad.Players.[playerId] <- { player with PlayerType = playerType }

                applyEvents t (Some(squad, rvn.NextRvn))
            else
                Error $"Invalid {nameof SquadEvent}: {nameof PlayerTypeChanged} when not added"
        | Some(squad, rvn), PlayerWithdrawn(_, playerId, dateWithdrawn) :: t ->
            if squad.Players.ContainsKey playerId then
                let player = squad.Players.[playerId]

                squad.Players.[playerId] <- {
                    player with
                        PlayerStatus = Withdrawn dateWithdrawn
                }

                applyEvents t (Some(squad, rvn.NextRvn))
            else
                Error $"Invalid {nameof SquadEvent}: {nameof PlayerWithdrawn} when not added"
        | Some(squad, rvn), SquadEliminated _ :: t ->
            applyEvents t (Some({ squad with Eliminated = true }, rvn.NextRvn))
        | Some squadAndRvn, [] -> Ok squadAndRvn
        | None, [] -> Error $"No initial {nameof SquadEvent}"
        | None, h :: _ -> Error $"Invalid initial {nameof SquadEvent}: {h}"
        | Some _, SquadCreated _ :: _ -> Error $"Invalid non-initial {nameof SquadEvent}: {nameof SquadCreated}"

    interface IHelper<SquadEvent<'group, 'playerType>, Squad<'group, 'playerType>> with
        member _.ApplyEvents events = applyEvents events None
