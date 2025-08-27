namespace Aornota.Ubersweep.Migration

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common

open FsToolkit.ErrorHandling
open System
open System.IO
open Thoth.Json.Net

type PersistedEvent = { // note: not private as this would cause decoding errors
    Rvn: Rvn
    TimestampUtc: DateTime
    EventJson: Json
    AuditUserId: UserId'
}

type private DeserializationHelper<'event>(eventDecoder: Decoder<'event>, useLegacyDeserializer) =
    let persistedEventDecoder =
        Decode.Auto.generateDecoderCached<PersistedEvent> (Json.caseStrategy, Json.extraCoders)

    member _.DecodePersistedEvent(Json json) =
        if useLegacyDeserializer then
            LegacyDeserializer.fromJson<PersistedEvent> (Json json)
        else
            Decode.fromString persistedEventDecoder json

    member _.DecodeEvent<'event>(Json json) =
        if useLegacyDeserializer then
            LegacyDeserializer.fromJson<'event> (Json json)
        else
            Decode.fromString eventDecoder json

type Event<'event> = {
    Rvn: Rvn
    TimestampUtc: DateTime
    Event: 'event
    AuditUserId: UserId'
}

type Reader<'event>(path: string, eventDecoder: Decoder<'event>, useLegacyDeserializer, logger) =
    [<Literal>]
    let fileExtension = "events"

    let logger = SourcedLogger.Create<Reader<_>>(sanitize typeof<'event>, logger)

    do logger.Information("Using path: {path}", DirectoryInfo(path).FullName)

    let deserializationHelper =
        DeserializationHelper<'event>(eventDecoder, useLegacyDeserializer)

    let tryDecodeFileAsync (file: FileInfo) = asyncResult {
        let rec checkPersistedEvents previousRvn (persistedEvents: PersistedEvent list) =
            match persistedEvents, previousRvn with
            | { Rvn = rvn } :: t, None ->
                if rvn = Rvn.InitialRvn then
                    checkPersistedEvents (Some rvn) t
                else
                    Error $"{nameof Rvn} of first {nameof PersistedEvent} ({rvn}) is not {Rvn.InitialRvn}"
            | { Rvn = rvn } :: t, Some previousRvn ->
                if rvn = previousRvn.NextRvn then
                    checkPersistedEvents (Some rvn) t
                else
                    Error
                        $"{nameof Rvn} of {nameof PersistedEvent} ({rvn}) not contiguous with previous {nameof PersistedEvent} ({previousRvn})"
            | [], None -> Error $"File {file.Name} has no decoded {nameof PersistedEvent}s" // should never happen as logic in calling code below ensures that persistedEvents is not empty on the first call
            | [], _ -> Ok()

        try
            let! lines = File.ReadAllLinesAsync file.FullName

            match lines |> List.ofArray with
            | [] -> return! Error $"File {file.Name} is empty"
            | lines ->
                match
                    lines
                    |> List.map (fun line -> deserializationHelper.DecodePersistedEvent(Json line))
                    |> List.sequenceResultA
                with
                | Ok persistedEvents ->
                    let! _ = // error if persisted events are invalid
                        checkPersistedEvents None persistedEvents

                    return! Ok persistedEvents
                | Error errors -> return! Error $"One or more decoding error for file {file.Name}: {errors}"
        with exn ->
            return! Error $"Exception when decoding file {file.Name}: {exn.Message}"
    }

    let tryReadAsync (guid: Guid) = asyncResult {
        try
            let file = FileInfo(Path.Combine(path, $"{guid}.{fileExtension}"))

            let! _ = // error if file does not exist
                if not (File.Exists file.FullName) then
                    Error $"File does not exist when reading {guid} for {path}"
                else
                    Ok()

            let! persistedEvents = async {
                let! result = tryDecodeFileAsync file

                match result with
                | Ok persistedEvents -> return Ok persistedEvents
                | Error error -> return Error $"Error when reading {guid} for {path}: {error}"
            }

            match
                persistedEvents
                |> List.map (fun persistedEvent ->
                    deserializationHelper.DecodeEvent persistedEvent.EventJson
                    |> Result.map (fun event -> {
                        Rvn = persistedEvent.Rvn
                        TimestampUtc = persistedEvent.TimestampUtc
                        Event = event
                        AuditUserId = persistedEvent.AuditUserId
                    }))
                |> List.sequenceResultA
            with
            | Ok events -> return! Ok events
            | Error errors -> return! Error $"One or more decoding error when reading {guid} for {path}: {errors}"
        with exn ->
            return! Error $"Exception when reading {guid} for {path}: {exn.Message}"
    }

    let tryReadAllAsync () = asyncResult {
        try
            let! _ = // error if path does not exist
                if not (Directory.Exists path) then
                    Error $"{path} does not exist when reading all"
                else
                    Ok()

            let fileAndGuids =
                (DirectoryInfo path).GetFiles $"*.{fileExtension}"
                |> List.ofArray
                |> List.map (fun file ->
                    match Guid.TryParse(Path.GetFileNameWithoutExtension file.Name) with
                    | true, guid -> file, Some guid
                    | false, _ -> file, None)

            let! guids =
                match
                    fileAndGuids
                    |> List.choose (fun (file, guid) -> if guid.IsNone then Some file.Name else None)
                with
                | [] -> Ok(fileAndGuids |> List.choose snd)
                | fileNames ->
                    Error $"Non-{nameof Guid} .{fileExtension} files exist when reading all for {path}: {fileNames}"

            let! results =
                guids
                |> List.sort
                |> List.map (fun guid -> tryReadAsync guid |> AsyncResult.map (fun list -> guid, list))
                |> Async.Parallel

            return!
                results
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.mapError (fun errors -> $"One or more error when reading all for {path}: {errors}")
        with exn ->
            return! Error $"Exception when reading all for {path}: {exn.Message}"
    }

    member _.ReadAllAsync() = async {
        logger.Debug("Reading {type}s for all...", sanitize typeof<'event>)

        let! result = tryReadAllAsync ()

        match result with
        | Ok list -> logger.Debug("...{type}s read for {length} file/s", sanitize typeof<'event>, list.Length)
        | Error error -> logger.Error("...error when reading {type}s for all: {error}", sanitize typeof<'event>, error)

        return result
    }
