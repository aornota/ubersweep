namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System

type SquadInitEvent<'group> =
    | SquadCreated of squadName: string * group: 'group * seeding: Seeding option * coachName: string

    interface IEvent with
        member this.EventJson = Json.encode this

type SquadEvent<'playerType> =
    | PlayerAdded of playerId: PlayerId * playerName: string * playerType: 'playerType
    | PlayerNameChanged of playerId: PlayerId * playerName: string
    | PlayerTypeChanged of playerId: PlayerId * playerType: 'playerType
    | PlayerWithdrawn of playerId: PlayerId * dateWithdrawn: DateTimeOffset option
    | SquadEliminated

    interface IEvent with
        member this.EventJson = Json.encode this

type Squad<'group, 'playerType> = {
    SquadCommon: SquadCommon'<'group, 'playerType>
} with

    interface IState<Squad<'group, 'playerType>, SquadEvent<'playerType>> with
        member this.SnapshotJson = Json.encode this

        member this.Evolve event =
            match event with
            | PlayerAdded(playerId, playerName, playerType) ->
                match this.SquadCommon.Players |> Map.tryFind playerId with
                | Some _ -> Error $"{nameof PlayerAdded} when {playerId} already in {nameof Squad}"
                | None ->
                    let player = {
                        PlayerName = playerName
                        PlayerType = playerType
                        PlayerStatus = Active
                    }

                    Ok {
                        this with
                            SquadCommon.Players = this.SquadCommon.Players |> Map.add playerId player
                    }
            | PlayerNameChanged(playerId, playerName) ->
                match this.SquadCommon.Players |> Map.tryFind playerId with
                | Some player ->
                    Ok {
                        this with
                            SquadCommon.Players =
                                this.SquadCommon.Players
                                |> Map.remove playerId
                                |> Map.add playerId { player with PlayerName = playerName }
                    }
                | None -> Error $"{nameof PlayerNameChanged} when {playerId} not in {nameof Squad}"
            | PlayerTypeChanged(playerId, playerType) ->
                match this.SquadCommon.Players |> Map.tryFind playerId with
                | Some player ->
                    Ok {
                        this with
                            SquadCommon.Players =
                                this.SquadCommon.Players
                                |> Map.remove playerId
                                |> Map.add playerId { player with PlayerType = playerType }
                    }
                | None -> Error $"{nameof PlayerTypeChanged} when {playerId} not in {nameof Squad}"
            | PlayerWithdrawn(playerId, dateWithdrawn) ->
                match this.SquadCommon.Players |> Map.tryFind playerId with
                | Some player ->
                    Ok {
                        this with
                            SquadCommon.Players =
                                this.SquadCommon.Players
                                |> Map.remove playerId
                                |> Map.add playerId {
                                    player with
                                        PlayerStatus = Withdrawn dateWithdrawn
                                }
                    }
                | None -> Error $"{nameof PlayerWithdrawn} when {playerId} not in {nameof Squad}"
            | SquadEliminated ->
                Ok {
                    this with
                        SquadCommon.Eliminated = true
                }

type SquadHelper<'group, 'playerType>() =
    inherit
        EntityHelper<
            SquadId,
            Squad<'group, 'playerType>,
            SquadInitCommand<'group>,
            SquadInitEvent<'group>,
            SquadEvent<'playerType>
         >()

    override _.IdFromGuid guid = SquadId.FromGuid guid

    override _.InitFromCommand(guid, CreateSquad(squadName, group, seeding, coachName)) =
        {
            Id = SquadId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = {
                SquadCommon = {
                    SquadName = squadName
                    Group = group
                    Seeding = seeding
                    CoachName = coachName
                    Eliminated = false
                    Players = Map.empty<PlayerId, Player<'playerType>>
                }
            }
        },
        SquadCreated(squadName, group, seeding, coachName)

    override _.InitFromEvent(guid, SquadCreated(squadName, group, seeding, coachName)) = {
        Id = SquadId.FromGuid guid
        Rvn = Rvn.InitialRvn
        State = {
            SquadCommon = {
                SquadName = squadName
                Group = group
                Seeding = seeding
                CoachName = coachName
                Eliminated = false
                Players = Map.empty<PlayerId, Player<'playerType>>
            }
        }
    }

[<RequireQualifiedAccess>]
module Squad =
    let private decide (command: SquadCommand<'playerType>) (squad: Squad<'group, 'playerType>) =
        match command with
        | AddPlayer(playerId, playerName, playerType) ->
            match squad.SquadCommon.Players |> Map.tryFind playerId with
            | Some _ -> Error $"{nameof AddPlayer} when {playerId} already in {nameof Squad}"
            | None -> Ok(PlayerAdded(playerId, playerName, playerType))
        | ChangePlayerName(playerId, playerName) ->
            match squad.SquadCommon.Players |> Map.tryFind playerId with
            | Some _ -> Ok(PlayerNameChanged(playerId, playerName))
            | None -> Error $"{nameof ChangePlayerName} when {playerId} not in {nameof Squad}"
        | ChangePlayerType(playerId, playerType) ->
            match squad.SquadCommon.Players |> Map.tryFind playerId with
            | Some _ -> Ok(PlayerTypeChanged(playerId, playerType))
            | None -> Error $"{nameof ChangePlayerType} when {playerId} not in {nameof Squad}"
        | WithdrawPlayer(playerId, dateWithdrawn) ->
            match squad.SquadCommon.Players |> Map.tryFind playerId with
            | Some _ -> Ok(PlayerWithdrawn(playerId, dateWithdrawn))
            | None -> Error $"{nameof WithdrawPlayer} when {playerId} not in {nameof Squad}"
        | EliminateSquad ->
            if squad.SquadCommon.Eliminated then
                Error $"{nameof EliminateSquad} when {nameof Squad} already eliminated"
            else
                Ok SquadEliminated

    let helperEuro = SquadHelper<GroupAToF, PlayerTypeFootball>()
    let helperFifa = SquadHelper<GroupAToH, PlayerTypeFootball>()
    let helperRwc = SquadHelper<GroupAToD, PlayerTypeRugby>()

    let apply command (entity: Entity<SquadId, Squad<'group, 'playerType>, SquadEvent<'playerType>>) = result {
        let! event = decide command entity.State
        let! entity = entity.Evolve event
        return entity, event
    }
