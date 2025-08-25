namespace Aornota.Ubersweep.Migration

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common

open FsToolkit.ErrorHandling
open System
open System.IO

type PersistedEvent = {
    Rvn: Rvn
    TimestampUtc: DateTime
    EventJson: Json
    AuditUserId: UserId'
}

type Event<'event> = {
    Rvn: Rvn
    TimestampUtc: DateTime
    Event: 'event
    AuditUserId: UserId'
}

type private DeserializationHelper(useLegacyDeserializer) =
    member _.FromJson<'a> json =
        if useLegacyDeserializer then
            LegacyDeserializer.fromJson<'a> json
        else
            Json.fromJson<'a> json

type Reader<'event>(path: string, useLegacyDeserializer, logger) =
    [<Literal>]
    let fileExtension = "events"

    let logger = SourcedLogger.Create<Reader<_>>(sanitize typeof<'event>, logger)

    do logger.Information("Using path: {path}", DirectoryInfo(path).FullName)

    let deserializationHelper = DeserializationHelper useLegacyDeserializer

    let tryRead (guid: Guid) =
        let rec checkConsistency (persistedEvents: PersistedEvent list) lastRvn =
            match persistedEvents with
            | { Rvn = rvn } :: t ->
                if rvn.IsValidNextRvn lastRvn then
                    checkConsistency t (Some rvn)
                else
                    Error
                        $"{nameof PersistedEvent} with {rvn} inconsistent with previous {nameof PersistedEvent} ({lastRvn})"
            | [] -> Ok()

        try
            let file = FileInfo(Path.Combine(path, $"{guid}.{fileExtension}"))

            if not (File.Exists file.FullName) then
                Error $"File does not exist when reading {guid} for {path}"
            else
                let lines = File.ReadAllLines file.FullName

                match lines |> List.ofArray with
                | [] -> Error $"File exists but is empty when reading {guid} for {path}"
                | lines ->
                    let deserializationResults =
                        lines
                        |> List.map (fun line -> deserializationHelper.FromJson<PersistedEvent>(Json line))

                    match
                        deserializationResults
                        |> List.choose (fun result ->
                            match result with
                            | Ok _ -> None
                            | Error error -> Some error)
                    with
                    | h :: _ ->
                        Error
                            $"At least one line caused a deserialization error when reading {guid} for {path} (e.g. {h})"
                    | _ ->
                        let persistedEvents =
                            deserializationResults
                            |> List.choose (fun result ->
                                match result with
                                | Ok entry -> Some entry
                                | Error _ -> None)

                        match checkConsistency persistedEvents None with
                        | Ok() ->
                            let deserializationResults =
                                persistedEvents
                                |> List.map (fun persistedEvent ->
                                    deserializationHelper.FromJson<'event> persistedEvent.EventJson
                                    |> Result.map (fun event -> {
                                        Rvn = persistedEvent.Rvn
                                        TimestampUtc = persistedEvent.TimestampUtc
                                        Event = event
                                        AuditUserId = persistedEvent.AuditUserId
                                    }))

                            match
                                deserializationResults
                                |> List.choose (fun result ->
                                    match result with
                                    | Ok _ -> None
                                    | Error error -> Some error)
                            with
                            | h :: _ ->
                                Error
                                    $"At least one {nameof PersistedEvent} caused a deserialization error when reading {guid} for {path} (e.g. {h})"
                            | _ ->
                                let events =
                                    deserializationResults
                                    |> List.choose (fun result ->
                                        match result with
                                        | Ok event -> Some event
                                        | Error _ -> None)

                                Ok events
                        | Error error -> Error $"Consistency check failed when reading {guid} for {path}: {error}"
        with exn ->
            Error $"Unexpected error reading {guid} for {path}: {exn.Message}"

    let tryReadAll () =
        try
            if Directory.Exists path then
                let files =
                    (DirectoryInfo path).GetFiles $"*.{fileExtension}"
                    |> List.ofArray
                    |> List.map (fun file ->
                        match Guid.TryParse(Path.GetFileNameWithoutExtension file.Name) with
                        | true, guid -> file, Some guid
                        | false, _ -> file, None)

                match
                    files
                    |> List.choose (fun (file, guid) -> if guid.IsNone then Some file else None)
                with
                | h :: _ ->
                    Error [
                        $"At least one .{fileExtension} file in {path} has a non-{nameof Guid} name (e.g. {h.Name})"
                    ]
                | _ ->
                    files
                    |> List.choose snd
                    |> List.sort
                    |> List.map (fun guid -> tryRead guid |> Result.map (fun list -> guid, list))
                    |> List.sequenceResultA
            else
                Error [ $"{path} does not exist when reading all" ]
        with exn ->
            Error [ $"Unexpected error reading all for {path}: {exn.Message}" ]

    member _.ReadAll() =
        logger.Debug("Reading {type}s for all...", sanitize typeof<'event>)

        let result = tryReadAll ()

        match result with
        | Ok list -> logger.Debug("...{type}s read for {length} file/s", sanitize typeof<'event>, list.Length)
        | Error errors ->
            logger.Error(
                "...{length} error/s reading {type}s for all: {errors}",
                errors.Length,
                sanitize typeof<'event>,
                errors
            )

        result
