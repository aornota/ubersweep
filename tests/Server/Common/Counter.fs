namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Shared.Common

open FsToolkit.ErrorHandling
open System
open Thoth.Json.Net

type CounterId =
    private
    | CounterId of guid: Guid

    static member Create() = CounterId(Guid.NewGuid())
    static member FromGuid guid = CounterId guid

    interface IId with
        member this.Guid =
            let (CounterId guid) = this
            guid

type CounterInitCommand = Initialize of count: int

type CounterCommand =
    | Increment
    | Decrement
    | MultiplyBy of multiplier: int
    | DivideBy of divisor: int

type CounterInitEvent =
    | Initialized of count: int

    interface IEvent with
        member this.EventJson = Json.encode this

type CounterEvent =
    | Incremented
    | Decremented
    | MultipliedBy of multiplier: int
    | DividedBy of divisor: int

    interface IEvent with
        member this.EventJson = Json.encode this

type Counter = {
    Count: int
} with

    interface IState<Counter, CounterEvent> with
        member this.SnapshotJson = Json.encode this

        member this.Evolve event =
            match event with
            | Incremented -> Ok { this with Count = this.Count + 1 }
            | Decremented -> Ok { this with Count = this.Count - 1 }
            | MultipliedBy multiplier ->
                Ok {
                    this with
                        Count = this.Count * multiplier
                }
            | DividedBy divisor ->
                Ok {
                    this with
                        Count = this.Count / divisor
                }

type CounterEventHelper() =
    inherit EntityHelper<CounterId, Counter, CounterInitCommand, CounterInitEvent, CounterEvent>()

    let eventDecoder =
        Decode.Auto.generateDecoderCached<CounterEvent> (Json.caseStrategy, Json.extraCoders)

    let initEventDecoder =
        Decode.Auto.generateDecoderCached<CounterInitEvent> (Json.caseStrategy, Json.extraCoders)

    let stateDecoder =
        Decode.Auto.generateDecoderCached<Counter> (Json.caseStrategy, Json.extraCoders)

    override _.DecodeEvent(Json json) = Decode.fromString eventDecoder json
    override _.DecodeInitEvent(Json json) = Decode.fromString initEventDecoder json
    override _.DecodeState(Json json) = Decode.fromString stateDecoder json
    override _.IdFromGuid guid = CounterId.FromGuid guid

    override _.InitFromCommand(guid, Initialize count) =
        {
            Id = CounterId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = { Count = count }
        },
        Initialized count

    override _.InitFromEvent(guid, Initialized count) = {
        Id = CounterId.FromGuid guid
        Rvn = Rvn.InitialRvn
        State = { Count = count }
    }

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

    let helper = CounterEventHelper()

    let apply command (entity: Entity<CounterId, Counter, CounterEvent>) = result {
        let! event = decide command entity.State
        let! entity = entity.Evolve event
        return entity, event
    }
