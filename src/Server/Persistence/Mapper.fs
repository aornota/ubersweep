namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain

open FsToolkit.ErrorHandling
open System

[<AbstractClass>]
type MapperHelper<'entity, 'state, 'initEvent, 'event
    when 'entity :> Entity<'state>
    and 'state :> IState
    and 'state: equality
    and 'initEvent :> IEvent
    and 'event :> IEvent>() =
    abstract member InitializeFromEvent: Guid * 'initEvent -> 'entity
    abstract member Make: Guid * Rvn * 'state -> 'entity
    abstract member Evolve: 'entity -> 'event -> 'entity

type Mapper<'entity, 'state, 'initEvent, 'event
    when 'entity :> Entity<'state>
    and 'state :> IState
    and 'state: equality
    and 'initEvent :> IEvent
    and 'event :> IEvent>(helper: MapperHelper<'entity, 'state, 'initEvent, 'event>) =
    member _.FromEntries(guid, entries: NonEmptyList<Entry>) = result {
        let rec processEntries eventEntries events =
            match eventEntries with
            | h :: t ->
                match h with
                | EventJson(_, _, _, json) ->
                    match Json.fromJson<'event> json with
                    | Ok event -> processEntries t (event :: events)
                    | Error error -> Error error
                | SnapshotJson _ -> Error $"{nameof eventEntries} contains a {nameof SnapshotJson}"
            | [] -> Ok(events |> List.rev)

        let! entity, eventEntries =
            match entries.Head with
            | SnapshotJson(rvn, json) -> result {
                let! state = Json.fromJson<'state> json
                return helper.Make(guid, rvn, state), entries.Tail
              }
            | EventJson(_, _, _, json) -> result {
                let! initEvent = Json.fromJson<'initEvent> json
                return helper.InitializeFromEvent(guid, initEvent), entries.Tail
              }

        let! events = processEntries eventEntries []

        return events |> List.fold helper.Evolve entity
    }
