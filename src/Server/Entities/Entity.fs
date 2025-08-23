namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Persistence

open FsToolkit.ErrorHandling
open System

type Entity<'id, 'state, 'event when 'id :> IId and 'state :> IState<'state, 'event> and 'event :> IEvent> = {
    Id: 'id
    Rvn: Rvn
    State: 'state
} with

    member this.Guid = this.Id.Guid
    member this.SnapshotJson = this.State.SnapshotJson

    member this.Evolve event = result {
        let! state = this.State.Evolve event

        return!
            Ok {
                this with
                    Rvn = this.Rvn.NextRvn
                    State = state
            }
    }

[<AbstractClass>]
type EntityHelper<'id, 'state, 'initCommand, 'initEvent, 'event
    when 'id :> IId and 'state :> IState<'state, 'event> and 'initEvent :> IEvent and 'event :> IEvent>() =
    abstract member IdFromGuid: Guid -> 'id
    abstract member InitFromCommand: Guid * 'initCommand -> Entity<'id, 'state, 'event> * 'initEvent
    abstract member InitFromEvent: Guid * 'initEvent -> Entity<'id, 'state, 'event>

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

        let folder (entity: Result<Entity<'id, 'state, 'event>, string>) event =
            entity |> Result.bind (fun entity -> entity.Evolve event)

        let! entityFromFirstEntry, subsequentEntries =
            match entries.Head with
            | SnapshotJson(rvn, json) -> result {
                let! state = Json.fromJson<'state> json

                return
                    {
                        Id = this.IdFromGuid guid
                        Rvn = rvn
                        State = state
                    },
                    entries.Tail
              }
            | EventJson(_, _, _, json) -> result {
                let! initEvent = Json.fromJson<'initEvent> json
                return this.InitFromEvent(guid, initEvent), entries.Tail
              }

        let! events = mapToEvents subsequentEntries []

        return events |> List.fold folder (Ok entityFromFirstEntry)
    }
