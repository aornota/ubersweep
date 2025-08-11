namespace Aornota.Ubersweep.Tests.Server

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Domain
open Aornota.Ubersweep.Tests.Server.Common

open Expecto
open FsToolkit.ErrorHandling

[<RequireQualifiedAccess>]
module FileReaderAndWriterTests =
    let private auditUser1Id = EntityId<UserState>.Initialize None
    let private auditUser2Id = EntityId<UserState>.Initialize None

    let private happyTests =
        testList "Happy tests" [
            testAsync "WIP test" {
                use testDir = new TestPersistenceDirectory<Counter, CounterState>(None, Some 4u)

                let! result = asyncResult {
                    let expected, event = Counter.InitializeFromCommand(Initialize -1)
                    let! _ = testDir.WriteAsync(expected, event, auditUser1Id)
                    let! expected, event = expected |> Counter.apply Increment
                    let! _ = testDir.WriteAsync(expected, event, auditUser1Id)
                    let! expected, event = expected |> Counter.apply Increment
                    let! _ = testDir.WriteAsync(expected, event, auditUser1Id)
                    let! expected, event = expected |> Counter.apply Increment
                    let! _ = testDir.WriteAsync(expected, event, auditUser1Id)
                    let! expected, event = expected |> Counter.apply (MultiplyBy 2)
                    let! _ = testDir.WriteAsync(expected, event, auditUser2Id)
                    let! expected, event = expected |> Counter.apply Increment
                    let! _ = testDir.WriteAsync(expected, event, auditUser1Id)
                    let! expected, event = expected |> Counter.apply (MultiplyBy 2)
                    let! _ = testDir.WriteAsync(expected, event, auditUser2Id)
                    let! expected, event = expected |> Counter.apply Decrement
                    let! _ = testDir.WriteAsync(expected, event, auditUser1Id)

                    let guid = (expected :> IEntity<CounterState>).Id.Guid
                    let! entriesForGuid = testDir.ReadAsync guid
                    let! actualForGuid = Counter.mapper.FromEntries(guid, entriesForGuid)

                    let! all = testDir.ReadAllAsync()

                    let! guidAndEntriesForOnly =
                        match all with
                        | [ result ] -> result
                        | [] -> Error "Reading all returned no items"
                        | _ -> Error "Reading all returned multiple itens"

                    let! actualForOnly = Counter.mapper.FromEntries guidAndEntriesForOnly

                    return actualForGuid, actualForOnly, expected
                }

                match result with
                | Ok(actualForGuid, actualForOnly, expected) ->
                    let expectedCount = 9

                    Expect.equal
                        expected.State.Count
                        expectedCount
                        $"Count for {nameof expected} should equal {expectedCount}"

                    Expect.equal actualForGuid expected $"{nameof actualForGuid} shoudl equal {nameof expected}"
                    Expect.equal actualForOnly expected $"{nameof actualForOnly} shoudl equal {nameof expected}"
                | Error _ -> Expect.isOk result $"{nameof result} should be Ok"
            }
        // TODO: More happy tests
        ]

    // TOD: Sad tests...

    let tests = testList $"{nameof FileReaderAndWriter} tests" [ happyTests ]
