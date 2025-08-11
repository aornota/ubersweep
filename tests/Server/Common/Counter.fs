namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain

open FsToolkit.ErrorHandling

(* For domain entities:
     -- CounterInitCommand | CounterCommand | Counter would be defined in Shared project?
     -- CounterInitEvent | CounterEvent | CounterMapper would be defined in Server project? *)

type CounterInitCommand = Initialize of count: int

type CounterCommand =
    | Increment
    | Decrement
    | MultiplyBy of multiplier: int
    | DivideBy of divisor: int

type Counter = {
    Count: int
} with

    interface IState with
        member this.SnapshotJson = Json.toJson this

type CounterInitEvent =
    | Initialized of count: int

    interface IEvent with
        member this.EventJson = Json.toJson this

type CounterEvent =
    | Incremented
    | Decremented
    | MultipliedBy of multiplier: int
    | DividedBy of divisor: int

    interface IEvent with
        member this.EventJson = Json.toJson this

type CounterHelper() =
    inherit EntityHelper<Counter, CounterInitEvent, CounterEvent>()

    override _.InitializeFromEvent(guid, Initialized count) =
        Entity<Counter>(EntityId<Counter>.Initialize(Some guid), Rvn.InitialRvn, { Count = count })

    override _.Evolve entity event =
        let state =
            match event with
            | Incremented -> {
                entity.State with
                    Count = entity.State.Count + 1
              }
            | Decremented -> {
                entity.State with
                    Count = entity.State.Count - 1
              }
            | MultipliedBy multiplier -> {
                entity.State with
                    Count = entity.State.Count * multiplier
              }
            | DividedBy divisor -> {
                entity.State with
                    Count = entity.State.Count / divisor
              }

        (entity :> IEntity<Counter>).Evolve state

        entity

    member _.InitializeFromCommand(Initialize count) =
        Entity<Counter>(EntityId<Counter>.Initialize None, Rvn.InitialRvn, { Count = count }), Initialized count

[<RequireQualifiedAccess>]
module Counter =
    let private decide command (_: Entity<Counter>) =
        match command with
        | Increment -> Ok Incremented
        | Decrement -> Ok Decremented
        | MultiplyBy multiplier -> Ok(MultipliedBy multiplier)
        | DivideBy divisor ->
            if divisor <> 0 then
                Ok(DividedBy divisor)
            else
                Error "Cannot divide by zero"

    let helper = CounterHelper()

    let apply command entity = result {
        let! event = decide command entity
        return helper.Evolve entity event, event
    }

    let mapper = EntityMapper<Counter, CounterInitEvent, CounterEvent> helper
