namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Entities

open Expecto

[<AutoOpen>]
module Common =
    let fixedUtcNow = (FixedClock.instance :> IPersistenceClock).GetUtcNow()

    let auditUser1Id = UserId.Create()
    let auditUser2Id = UserId.Create()

    let sourceUser1 = User auditUser1Id
    let sourceUser2 = User auditUser2Id

[<RequireQualifiedAccess>]
module Check =
    let private incorrectResult = $"Unexpected {nameof Result}"

    let equal (expected: 'a) (actual: 'a) =
        Expect.equal actual expected $"Unexpected value"

    let isOk (expectedOk: 'a) (result: Result<'a, 'b>) =
        match result with
        | Ok actualOk -> Expect.equal actualOk expectedOk $"Unexpected {nameof Ok} value"
        | Error _ -> Expect.isOk result incorrectResult

    let isError (expectedError: 'b) (result: Result<'a, 'b>) =
        match result with
        | Ok _ -> Expect.isError result incorrectResult
        | Error actualError -> Expect.equal actualError expectedError $"Unexpected {nameof Error} value"
