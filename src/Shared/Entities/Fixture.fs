namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type FixtureId =
    private
    | FixtureId of guid: Guid

    static member Create() = FixtureId(Guid.NewGuid())
    static member FromGuid guid = FixtureId guid

    interface IId with
        member this.Guid =
            let (FixtureId guid) = this
            guid

type MatchEventId =
    private
    | MatchEventId of guid: Guid

    static member Create() = MatchEventId(Guid.NewGuid())
    static member FromGuid guid = MatchEventId guid

    interface IId with
        member this.Guid =
            let (MatchEventId guid) = this
            guid

type Role =
    | Home
    | Away

type StageEuro =
    | Group of group: GroupAToF
    | RoundOf16 of matchNumber: uint32
    | QuarterFinal of quarterFinalOrdinal: uint32
    | SemiFinal of semiFinalOrdinal: uint32
    | Final

type StageFifa =
    | Group of group: GroupAToH
    | RoundOf16 of matchNumber: uint32
    | QuarterFinal of quarterFinalOrdinal: uint32
    | SemiFinal of semiFinalOrdinal: uint32
    | ThirdPlacePlayOff
    | Final

type StageRwc =
    | Group of group: GroupAToD
    | QuarterFinal of quarterFinalOrdinal: uint32
    | SemiFinal of semiFinalOrdinal: uint32
    | BronzeFinal
    | Final

type UnconfirmedEuro =
    | Winner of stage: StageEuro
    | RunnerUp of group: GroupAToF
    | ThirdPlace of groups: GroupAToF list

type UnconfirmedFifa =
    | Winner of stage: StageFifa
    | RunnerUp of group: GroupAToH
    | Loser of stage: StageFifa

type UnconfirmedRwc =
    | Winner of stage: StageRwc
    | RunnerUp of group: GroupAToD
    | Loser of stage: StageRwc

type Participant<'unconfirmed> =
    | Confirmed of squadId: SquadId
    | Unconfirmed of unconfirmed: 'unconfirmed

type PenaltyOutcome =
    | Scored
    | Missed
    | Saved of savedBy: SquadId * PlayerId

type MatchEventFootball =
    | Goal of squadId: SquadId * playerId: PlayerId * assistedBy: PlayerId option
    | OwnGoal of squadId: SquadId * playerId: PlayerId
    | Penalty of squadId: SquadId * playerId: PlayerId * penaltyOutcome: PenaltyOutcome
    | YellowCard of squadId: SquadId * playerId: PlayerId
    | RedCard of squadId: SquadId * playerId: PlayerId
    | CleanSheet of squadId: SquadId * playerId: PlayerId
    | PenaltyShootout of homeScore: uint32 * awayScore: uint32
    | ManOfTheMatch of squadId: SquadId * playerId: PlayerId

type KickOutcome =
    | Successful
    | Missed

type MatchEventRugby =
    | Try of squadId: SquadId * playerId: PlayerId
    | PenaltyTry of squadId: SquadId
    | PenaltyKick of squadId: SquadId * playerId: PlayerId * kickOutcome: KickOutcome
    | Conversion of squadId: SquadId * playerId: PlayerId * kickOutcome: KickOutcome
    | DropGoal of squadId: SquadId * playerId: PlayerId
    | YellowCard of squadId: SquadId * playerId: PlayerId
    | RedCard of squadId: SquadId * playerId: PlayerId
    | ManOfTheMatch of squadId: SquadId * playerId: PlayerId
