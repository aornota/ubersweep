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

type Fixture<'stage, 'unconfirmed, 'matchEvent> = { // TODO-ENTITIES: Implement this properly...
    Dummy: unit
} with

    interface IState<Fixture<'stage, 'unconfirmed, 'matchEvent>, FixtureEvent<'matchEvent>> with
        member this.SnapshotJson = Json.toJson this

        member this.Evolve event = Ok this
