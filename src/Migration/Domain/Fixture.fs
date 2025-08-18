namespace Aornota.Ubersweep.Migration.Domain

(*
open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Shared.Common
*)

open System

type FixtureId =
    | FixtureId of guid: Guid

    static member Create() = Guid.NewGuid() |> FixtureId

type Role =
    | Home
    | Away

type StageEuro =
    | Group of group: Group6
    | RoundOf16 of matchNumber: uint32
    | QuarterFinal of quarterFinalOrdinal: uint32
    | SemiFinal of semiFinalOrdinal: uint32
    | Final

type StageFifa =
    | Group of group: Group8
    | RoundOf16 of matchNumber: uint32
    | QuarterFinal of quarterFinalOrdinal: uint32
    | SemiFinal of semiFinalOrdinal: uint32
    | ThirdPlacePlayOff
    | Final

type StageRwc =
    | Group of group: Group4
    | QuarterFinal of quarterFinalOrdinal: uint32
    | SemiFinal of semiFinalOrdinal: uint32
    | BronzeFinal
    | Final

type UnconfirmedEuro =
    | Winner of stage: StageEuro
    | RunnerUp of group: Group6
    | ThirdPlace of groups: Group6 list

type UnconfirmedFifa =
    | Winner of stage: StageFifa
    | RunnerUp of group: Group8
    | Loser of semiFinalOrdinal: uint32

type UnconfirmedFifaV2 =
    | Winner of stage: StageFifa
    | RunnerUp of group: Group8
    | Loser of stage: StageFifa

type UnconfirmedRwc =
    | GroupRunnerUp of group: Group4
    | StageWinner of stage: StageRwc
    | SemiFinalLoser of semiFinalOrdinal: uint32

type Participant<'unconfirmed> =
    | Confirmed of squadId: SquadId
    | Unconfirmed of unconfirmed: 'unconfirmed

type MatchEventId =
    | MatchEventId of guid: Guid

    static member Create() = Guid.NewGuid() |> MatchEventId

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

(*
type PenaltyShootoutOutcome = { HomeScore: uint32; AwayScore: uint32 }

type MatchOutcomeFootball = {
    HomeGoals: uint32
    AwayGoals: uint32
    PenaltyShootoutOutcome: PenaltyShootoutOutcome option
}

type MatchOutcomeRugby = {
    HomeScore: uint32
    AwayScore: uint32
    HomeTotalTries: uint32
    AwayTotalTries: uint32
    HomePenaltyTries: uint32
    AwayPenaltyTries: uint32
}

type Card =
    | Yellow
    | SecondYellow
    | Red

type TeamScoreEventFootball =
    | MatchWon
    | MatchDrawn
    | PlayerCard of playerId: PlayerId * card: Card

type TeamScoreEventRugby =
    | MatchWon
    | MatchDrawn
    | TriesBonusPoint
    | LosingBonusPoint
    | PenaltyTryScored
    | PlayerCard of playerId: PlayerId * card: Card

type PlayerScoreEventFootball =
    | GoalScored
    | GoalAssisted
    | OwnGoalScored
    | PenaltyScored
    | PenaltyMissed
    | Card of card: Card
    | PenaltySaved
    | CleanSheetKept
    | ManOfTheMatchAwarded

type PlayerScoreEventRugby =
    | TryScored
    | PenaltyKickSuccessful
    | PenaltyKickMissed
    | ConversionSuccessful
    | ConversionMissed
    | DropGoalSuccessful
    | Card of card: Card
    | ManOfTheMatchAwarded

type ScoreEvents<'teamScoreEvent, 'playerScoreEvent> = {
    TeamScoreEvents: ('teamScoreEvent * int<point>) list
    PlayerScoreEvents: (PlayerId * ('playerScoreEvent * int<point>) list) list
}

type MatchResult<'matchEvent, 'matchOutcome, 'teamScoreEvent, 'playerScoreEvent> = {
    MatchOutcome: 'matchOutcome
    HomeScoreEvents: ScoreEvents<'teamScoreEvent, 'playerScoreEvent>
    AwayScoreEvents: ScoreEvents<'teamScoreEvent, 'playerScoreEvent>
    MatchEvents: (MatchEventId * 'matchEvent) list
}

type FixtureDto<'stage, 'unconfirmed, 'matchEvent, 'matchOutcome, 'teamScoreEvent, 'playerScoreEvent> = {
    FixtureId: FixtureId
    Rvn: Rvn
    Stage: 'stage
    HomeParticipant: Participant<'unconfirmed>
    AwayParticipant: Participant<'unconfirmed>
    KickOff: DateTimeOffset
    MatchResult: MatchResult<'matchEvent, 'matchOutcome, 'teamScoreEvent, 'playerScoreEvent> option
}
*)
