namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common

open System
open System.Collections.Generic

type FixtureEvent<'stage, 'unconfirmed, 'matchEvent> =
    | FixtureCreated of
        fixtureId: FixtureId *
        stage: 'stage *
        homeParticipant: Participant<'unconfirmed> *
        awayParticipant: Participant<'unconfirmed> *
        kickOff: DateTimeOffset
    | ParticipantConfirmed of fixtureId: FixtureId * role: Role * squadId: SquadId
    | MatchEventAdded of fixtureId: FixtureId * matchEventId: MatchEventId * matchEvent: 'matchEvent
    | MatchEventRemoved of fixtureId: FixtureId * matchEventId: MatchEventId
    | FixtureCancelled of fixtureId: FixtureId

type Fixture<'stage, 'unconfirmed, 'matchEvent> = {
    Stage: 'stage
    HomeParticipant: Participant<'unconfirmed>
    AwayParticipant: Participant<'unconfirmed>
    KickOff: DateTimeOffset
    Cancelled: bool
    MatchEvents: Dictionary<MatchEventId, 'matchEvent>
}

type FixtureHelper<'stage, 'unconfirmed, 'matchEvent>() =
    let rec applyEvents (events: FixtureEvent<'stage, 'unconfirmed, 'matchEvent> list) fixturesAndRvn =
        match fixturesAndRvn, events with
        | None, FixtureCreated(_, stage, homeParticipant, awayParticipant, kickOff) :: t ->
            applyEvents
                t
                (Some(
                    {
                        Stage = stage
                        HomeParticipant = homeParticipant
                        AwayParticipant = awayParticipant
                        KickOff = kickOff
                        Cancelled = false
                        MatchEvents = Dictionary<MatchEventId, 'matchEvent>()
                    },
                    Rvn.InitialRvn
                ))
        | Some(fixture, rvn), ParticipantConfirmed(_, role, squadId) :: t ->
            let fixture =
                match role with
                | Home -> {
                    fixture with
                        HomeParticipant = Confirmed squadId
                  }
                | Away -> {
                    fixture with
                        AwayParticipant = Confirmed squadId
                  }

            applyEvents t (Some(fixture, rvn.NextRvn))
        | Some(fixture, rvn), MatchEventAdded(_, matchEventId, matchEvent) :: t ->
            if not (fixture.MatchEvents.ContainsKey matchEventId) then
                fixture.MatchEvents.Add(matchEventId, matchEvent)

                applyEvents t (Some(fixture, rvn.NextRvn))
            else
                Error $"Invalid {nameof FixtureEvent}: {nameof MatchEventAdded} when already added"
        | Some(fixture, rvn), MatchEventRemoved(_, matchEventId) :: t ->
            if fixture.MatchEvents.ContainsKey matchEventId then
                fixture.MatchEvents.Remove matchEventId |> ignore

                applyEvents t (Some(fixture, rvn.NextRvn))
            else
                Error $"Invalid {nameof FixtureEvent}: {nameof MatchEventRemoved} when not added"
        | Some(fixture, rvn), FixtureCancelled _ :: t ->
            applyEvents t (Some({ fixture with Cancelled = true }, rvn.NextRvn))
        | Some fixturesAndRvn, [] -> Ok fixturesAndRvn
        | None, [] -> Error $"No initial {nameof FixtureEvent}"
        | None, h :: _ -> Error $"Invalid initial {nameof FixtureEvent}: {h}"
        | Some _, FixtureCreated _ :: _ -> Error $"Invalid non-initial {nameof FixtureEvent}: {nameof FixtureCreated}"

    interface IHelper<FixtureEvent<'stage, 'unconfirmed, 'matchEvent>, Fixture<'stage, 'unconfirmed, 'matchEvent>> with
        member _.ApplyEvents events = applyEvents events None
