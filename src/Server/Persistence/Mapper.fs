namespace Aornota.Ubersweep.Server.Persistence

open Aornota.Ubersweep.Server.Common.JsonConverter
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain

open FsToolkit.ErrorHandling

type Mapper<'entity, 'state, 'event when 'state :> IState and 'event :> IEvent>
    (helper: EntityHelper<'entity, 'state, 'event>) =
    member _.FromEntries(guid, entries: NonEmptyList<Entry>) = result {
        let rec checkEntries eventEntries events =
            match eventEntries with
            | h :: t ->
                match h with
                | EventJson(_, _, _, json) ->
                    match fromJson<'event> json with
                    | Ok event -> checkEntries t (event :: events)
                    | Error error -> Error error
                | SnapshotJson _ ->
                    Error $"{nameof entries} contains a {nameof SnapshotJson} that is not the first {nameof Entry}"
            | [] -> Ok(events |> List.rev)

        let! entity, eventEntries =
            match entries.Head with
            | SnapshotJson(rvn, json) -> result {
                let! state = fromJson<'state> json
                return helper.Make(guid, rvn, state), entries.Tail
              }
            | EventJson _ -> Ok(helper.Initialize(Some guid), entries.List)

        let! events = checkEntries eventEntries []

        return events |> List.fold helper.Evolve entity
    }
