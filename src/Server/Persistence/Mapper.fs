namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common.JsonConverter
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain

open FsToolkit.ErrorHandling
open System

[<AbstractClass>]
type MapperHelper<'entity, 'state, 'initEvent, 'event when 'initEvent :> IEvent<'initEvent> and 'event :> IEvent<'event>>
    () =
    abstract member InitializeFromEvent: Guid * 'initEvent -> 'entity
    abstract member Make: Guid * Rvn * 'state -> 'entity
    abstract member Evolve: 'entity -> 'event -> 'entity

type Mapper<'entity, 'state, 'initEvent, 'event
    when 'state :> IState<'state> and 'initEvent :> IEvent<'initEvent> and 'event :> IEvent<'event>>
    (helper: MapperHelper<'entity, 'state, 'initEvent, 'event>) =
    member _.FromEntries(guid, entries: NonEmptyList<Entry>) = result {
        let rec processEntries eventEntries events =
            match eventEntries with
            | h :: t ->
                match h with
                | EventJson(_, _, _, json) ->
                    match fromJson<'event> json with
                    | Ok event -> processEntries t (event :: events)
                    | Error error -> Error error
                | SnapshotJson _ -> Error $"{nameof eventEntries} contains a {nameof SnapshotJson}"
            | [] -> Ok(events |> List.rev)

        let! entity, eventEntries =
            match entries.Head with
            | SnapshotJson(rvn, json) -> result {
                let! state = fromJson<'state> json
                return helper.Make(guid, rvn, state), entries.Tail
              }
            | EventJson(_, _, _, json) -> result {
                let! initEvent = fromJson<'initEvent> json
                return helper.InitializeFromEvent(guid, initEvent), entries.Tail
              }

        let! events = processEntries eventEntries []

        return events |> List.fold helper.Evolve entity
    }
