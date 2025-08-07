namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Server.Common.JsonConverter
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain

open FsToolkit.ErrorHandling

type CounterState = {
    Count: int
} with

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

#nowarn "3536"

type Counter = private {
    Id': EntityId<Counter>
    Rvn': Rvn
    State': CounterState
} with

    interface IEntity<Counter, CounterState, CounterEvent> with
        static member Initialize guid = {
            Id' = EntityId<Counter>.Initialize guid
            Rvn' = Rvn.InitialRvn
            State' = { Count = 0 }
        }

        static member Make(guid, rvn, state) = {
            Id' = EntityId<Counter>.Initialize(Some guid)
            Rvn' = rvn
            State' = state
        }

        static member Evolve entity event =
            let entity' = entity :> IEntity<Counter, CounterState, CounterEvent>

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

        member this.Id = this.Id'
        member this.Rvn = this.Rvn'
        member this.State = this.State'

[<RequireQualifiedAccess>]
module Counter =
    let private decide command (_: CounterState) =
        match command with
        | Increment -> Ok Incremented
        | Decrement -> Ok Decremented

    let apply command (entity: Counter) = result {
        let! event = decide command (entity :> IEntity<Counter, CounterState, CounterEvent>).State
        return IEntity<Counter, CounterState, CounterEvent>.Evolve entity event
    }

    let mapper = Mapper<Counter, CounterState, CounterEvent>()
