namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Server.Common.JsonConverter
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain

open FsToolkit.ErrorHandling

type CounterState = {
    Count: int
} with

    static member InitialState = { Count = 0 }

    interface IState with
        member this.SnapshotJson = toJson this

type CounterCommand =
    | Increment
    | Decrement

type CounterEvent =
    | Incremented
    | Decremented

    interface IEvent with
        member this.EventJson = toJson this

type Counter = private {
    Id': EntityId<Counter>
    Rvn': Rvn
    State': CounterState
} with

    interface IEntity<Counter, CounterState, CounterEvent> with
        member this.Id = this.Id'
        member this.Rvn = this.Rvn'
        member this.State = this.State'

type ICounterEntity = IEntity<Counter, CounterState, CounterEvent>

type CounterHelper() =
    inherit EntityHelper<Counter, CounterState, CounterEvent>()

    override _.Initialize guid = {
        Id' = EntityId<Counter>.Initialize guid
        Rvn' = Rvn.InitialRvn
        State' = CounterState.InitialState
    }

    override _.Make(guid, rvn, state) = {
        Id' = EntityId<Counter>.Initialize(Some guid)
        Rvn' = rvn
        State' = state
    }

    override _.Evolve entity event =
        let entity' = entity :> ICounterEntity

        let state =
            match event with
            | Incremented -> {
                entity'.State with
                    Count = entity'.State.Count + 1
              }
            | Decremented -> {
                entity'.State with
                    Count = entity'.State.Count - 1
              }

        {
            entity with
                Rvn' = entity'.Rvn.NextRvn
                State' = state
        }

[<RequireQualifiedAccess>]
module Counter =
    let private decide command (_: CounterState) =
        match command with
        | Increment -> Ok Incremented
        | Decrement -> Ok Decremented

    let helper = CounterHelper()

    let apply command (entity: Counter) = result {
        let! event = decide command (entity :> ICounterEntity).State
        return helper.Evolve entity event, event
    }

    let mapper = Mapper<Counter, CounterState, CounterEvent> helper
