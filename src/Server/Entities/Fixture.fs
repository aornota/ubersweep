namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System

type FixtureInitEvent<'stage, 'unconfirmed> =
    | FixtureCreated of
        stage: 'stage *
        homeParticipant: Participant<'unconfirmed> *
        awayParticipant: Participant<'unconfirmed> *
        kickOff: DateTimeOffset

    interface IEvent with
        member this.EventJson = Json.toJson this

type FixtureEvent<'matchEvent> =
    | ParticipantConfirmed of role: Role * squadId: SquadId
    | MatchEventAdded of matchEventId: MatchEventId * matchEvent: 'matchEvent
    | MatchEventRemoved of matchEventId: MatchEventId
    | FixtureCancelled

    interface IEvent with
        member this.EventJson = Json.toJson this

type Fixture<'stage, 'unconfirmed, 'matchEvent> = {
    FixtureCommon: FixtureCommon'<'stage, 'unconfirmed, 'matchEvent>
} with

    interface IState<Fixture<'stage, 'unconfirmed, 'matchEvent>, FixtureEvent<'matchEvent>> with
        member this.SnapshotJson = Json.toJson this

        member this.Evolve event =
            match event with
            | ParticipantConfirmed(role, squadId) ->
                match role with
                | Home ->
                    Ok {
                        this with
                            FixtureCommon.HomeParticipant = Confirmed squadId
                    }
                | Away ->
                    Ok {
                        this with
                            FixtureCommon.AwayParticipant = Confirmed squadId
                    }
            | MatchEventAdded(matchEventId, matchEvent) ->
                match this.FixtureCommon.MatchEvents |> Map.tryFind matchEventId with
                | Some _ -> Error $"{nameof MatchEventAdded} when {matchEventId} already in {nameof Fixture}"
                | None ->
                    Ok {
                        this with
                            FixtureCommon.MatchEvents =
                                this.FixtureCommon.MatchEvents |> Map.add matchEventId matchEvent
                    }
            | MatchEventRemoved matchEventId ->
                match this.FixtureCommon.MatchEvents |> Map.tryFind matchEventId with
                | Some _ ->
                    Ok {
                        this with
                            FixtureCommon.MatchEvents = this.FixtureCommon.MatchEvents |> Map.remove matchEventId
                    }
                | None -> Error $"{nameof MatchEventAdded} when {matchEventId} not in {nameof Fixture}"
            | FixtureCancelled ->
                Ok {
                    this with
                        FixtureCommon.Cancelled = true
                }

type FixtureHelper<'stage, 'unconfirmed, 'matchEvent>() =
    inherit
        EntityHelper<
            FixtureId,
            Fixture<'stage, 'unconfirmed, 'matchEvent>,
            FixtureInitCommand<'stage, 'unconfirmed>,
            FixtureInitEvent<'stage, 'unconfirmed>,
            FixtureEvent<'matchEvent>
         >()

    override _.IdFromGuid guid = FixtureId.FromGuid guid

    override _.InitFromCommand(guid, CreateFixture(stage, homeParticipant, awayParticipant, kickOff)) =
        {
            Id = FixtureId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = {
                FixtureCommon = {
                    Stage = stage
                    HomeParticipant = homeParticipant
                    AwayParticipant = awayParticipant
                    KickOff = kickOff
                    Cancelled = false
                    MatchEvents = Map.empty<MatchEventId, 'matchEvent>
                }
            }
        },
        FixtureCreated(stage, homeParticipant, awayParticipant, kickOff)

    override _.InitFromEvent(guid, FixtureCreated(stage, homeParticipant, awayParticipant, kickOff)) = {
        Id = FixtureId.FromGuid guid
        Rvn = Rvn.InitialRvn
        State = {
            FixtureCommon = {
                Stage = stage
                HomeParticipant = homeParticipant
                AwayParticipant = awayParticipant
                KickOff = kickOff
                Cancelled = false
                MatchEvents = Map.empty<MatchEventId, 'matchEvent>
            }
        }
    }

[<RequireQualifiedAccess>]
module Fixture =
    let private decide (command: FixtureCommand<'matchEvent>) (fixture: Fixture<'stage, 'unconfirmed, 'matchEvent>) =
        match command with
        | ConfirmParticipant(role, squadId) ->
            match role, fixture.FixtureCommon.HomeParticipant, fixture.FixtureCommon.AwayParticipant with
            | Home, Confirmed _, _
            | Away, _, Confirmed _ ->
                Error
                    $"{nameof ConfirmParticipant} when {role} {nameof Participant} for {nameof Fixture} already {nameof Confirmed}"
            | Home, Unconfirmed _, _
            | Away, _, Unconfirmed _ -> Ok(ParticipantConfirmed(role, squadId))
        | AddMatchEvent(matchEventId, matchEvent) ->
            match fixture.FixtureCommon.MatchEvents |> Map.tryFind matchEventId with
            | Some _ -> Error $"{nameof AddMatchEvent} when {matchEventId} already in {nameof Fixture}"
            | None -> Ok(MatchEventAdded(matchEventId, matchEvent))
        | RemoveMatchEvent matchEventId ->
            match fixture.FixtureCommon.MatchEvents |> Map.tryFind matchEventId with
            | Some _ -> Ok(MatchEventRemoved matchEventId)
            | None -> Error $"{nameof AddMatchEvent} when {matchEventId} not in {nameof Fixture}"
        | CancelFixture ->
            if fixture.FixtureCommon.Cancelled then
                Error $"{nameof CancelFixture} when {nameof Fixture} already cancelled"
            else
                Ok FixtureCancelled

    let helperEuro = FixtureHelper<StageEuro, UnconfirmedEuro, MatchEventFootball>()

    let helperFifa =
        FixtureHelper<StageFifa, Unconfirmed<StageFifa, GroupAToH>, MatchEventFootball>()

    let helperRwc =
        FixtureHelper<StageRwc, Unconfirmed<StageRwc, GroupAToD>, MatchEventRugby>()

    let apply
        command
        (entity: Entity<FixtureId, Fixture<'stage, 'unconfirmed, 'matchEvent>, FixtureEvent<'matchEvent>>)
        =
        result {
            let! event = decide command entity.State
            let! entity = entity.Evolve event
            return entity, event
        }
