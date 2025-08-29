namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Entities

open Expecto
open System

type FileType =
    | Events
    | Snapshot

[<AutoOpen>]
module Common =
    let addFileExtension fileType (fileName: string) =
        match fileType with
        | Events -> $"{fileName}.{FilePersistence.eventsFileExtension}"
        | Snapshot -> $"{fileName}.{FilePersistence.snapshotFileExtension}"

    let fixedUtcNow = (FixedClock.instance :> IPersistenceClock).GetUtcNow()

    let userId1 = UserId.Create()
    let userId2 = UserId.Create()

    let sourceUser1 = User userId1
    let sourceUser2 = User userId2
    let sourceSystemTest = System "Test"

[<RequireQualifiedAccess>]
module Check =
    let private incorrectResult = $"Unexpected {nameof Result}"

    let equal (expected: 'a) (actual: 'a) =
        Expect.equal actual expected $"Unexpected value"

    let notEqual (expected: 'a) (actual: 'a) =
        Expect.notEqual actual expected $"Unexpected value"

    let isOk (expectedOk: 'a) (result: Result<'a, 'b>) =
        match result with
        | Ok actualOk -> Expect.equal actualOk expectedOk $"Unexpected {nameof Ok} value"
        | Error _ -> Expect.isOk result incorrectResult

    let isError (expectedError: 'b) (result: Result<'a, 'b>) =
        match result with
        | Ok _ -> Expect.isError result incorrectResult
        | Error actualError -> Expect.equal actualError expectedError $"Unexpected {nameof Error} value"

    let withOk (fOk: 'a -> unit) (result: Result<'a, 'b>) =
        match result with
        | Ok ok -> fOk ok
        | Error _ -> Expect.isOk result incorrectResult

    let withError (fError: 'b -> unit) (result: Result<'a, 'b>) =
        match result with
        | Ok _ -> Expect.isError result incorrectResult
        | Error error -> fError error
