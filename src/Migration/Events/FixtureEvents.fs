namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Domain

open System

type FixtureEvent =
    | FixtureCreated of
        fixtureId: FixtureId *
        stage: IStage *
        homeParticipant: Participant *
        awayParticipant: Participant *
        kickOff: DateTimeOffset // TODO: Cannot deserialize to interface?...
    | ParticipantConfirmed of fixtureId: FixtureId * role: Role * squadId: SquadId
    | MatchEventAdded of fixtureId: FixtureId * matchEventId: MatchEventId * matchEvent: IMatchEvent // TODO: Cannot deserialize to interface?...
    | MatchEventRemoved of fixtureId: FixtureId * matchEventId: MatchEventId
