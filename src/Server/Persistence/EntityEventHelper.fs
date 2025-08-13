namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System

[<AbstractClass>]
type EntityEventHelper<'id, 'entity, 'initEvent, 'event
    when 'id :> IId and 'entity :> IEntity and 'entity: equality and 'initEvent :> IEvent and 'event :> IEvent>() =
    abstract member InitializeFromEvent: Guid * 'initEvent -> Entity<'id, 'entity>
    abstract member Evolve: Entity<'id, 'entity> -> 'event -> Entity<'id, 'entity>

    member this.FromEntries(guid, entries: NonEmptyList<Entry>) = result {
        let rec mapToEvents subsequentEntries events =
            match subsequentEntries with
            | h :: t ->
                match h with
                | EventJson(_, _, _, json) ->
                    match Json.fromJson<'event> json with
                    | Ok event -> mapToEvents t (event :: events)
                    | Error error -> Error error
                | SnapshotJson _ -> Error $"Subsequent entries contain a {nameof SnapshotJson}"
            | [] -> Ok(events |> List.rev)

        let! entityFromFirstEntry, subsequentEntries =
            match entries.Head with
            | SnapshotJson(rvn, json) -> result {
                let! state = Json.fromJson<'entity> json
                return Entity<'id, 'entity>(EntityId<'id>.FromGuid guid, rvn, state), entries.Tail
              }
            | EventJson(_, _, _, json) -> result {
                let! initEvent = Json.fromJson<'initEvent> json
                return this.InitializeFromEvent(guid, initEvent), entries.Tail
              }

        let! events = mapToEvents subsequentEntries []

        return events |> List.fold this.Evolve entityFromFirstEntry
    }
