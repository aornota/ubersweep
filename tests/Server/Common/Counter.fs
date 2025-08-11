namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain

open FsToolkit.ErrorHandling

(* For domain entities:
     -- CounterInitCommand | CounterCommand would be defined in Shared project?
     -- CounterState | Counter might be defined in Shared project?
     -- CounterInitEvent | CounterEvent | CounterHelper would be defined in Server project? *)

type CounterInitCommand = Initialize of count: int

type CounterCommand =
    | Increment
    | Decrement
    | MultiplyBy of multiplier: int
    | DivideBy of divisor: int

type CounterState = {
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

type Counter private (id: EntityId<CounterState>, rvn: Rvn, state: CounterState) =
    inherit Entity<CounterState>(id, rvn, state)

    static let initialize guid state =
        Counter(EntityId<CounterState>.Initialize guid, Rvn.InitialRvn, state)

    static member InitializeFromCommand(Initialize count) =
        initialize None { Count = count }, Initialized count

    static member InitializeFromEvent(guid, Initialized count) =
        initialize (Some guid) { Count = count }

    static member Make(guid, rvn, state) =
        Counter(EntityId<CounterState>.Initialize(Some guid), rvn, state)

type CounterHelper() =
    inherit MapperHelper<Counter, CounterState, CounterInitEvent, CounterEvent>()

    override _.InitializeFromEvent(guid, Initialized count) =
        Counter.InitializeFromEvent(guid, Initialized count)

    override _.Make(guid, rvn, state) = Counter.Make(guid, rvn, state)

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

        (entity :> IEntity<CounterState>).Evolve state

        entity

[<RequireQualifiedAccess>]
module Counter =
    let private decide command (_: Counter) =
        match command with
        | Increment -> Ok Incremented
        | Decrement -> Ok Decremented
        | MultiplyBy multiplier -> Ok(MultipliedBy multiplier)
        | DivideBy divisor ->
            if divisor <> 0 then
                Ok(DividedBy divisor)
            else
                Error "Cannot divide by zero"

    let private helper = CounterHelper()

    let apply command entity = result {
        let! event = decide command entity
        return helper.Evolve entity event, event
    }

    let mapper = Mapper<Counter, CounterState, CounterInitEvent, CounterEvent> helper
