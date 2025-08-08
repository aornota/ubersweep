namespace Aornota.Ubersweep.Tests.Server

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Domain
open Aornota.Ubersweep.Tests.Server.Common

open Expecto
open FsToolkit.ErrorHandling
open System

[<RequireQualifiedAccess>]
module FileReaderAndWriterTests =
    let private getAgents (testDir: TestDirectory, snapshotFrequency) =
        new FileReaderAndWriter(
            testDir.Root,
            testDir.PartitionKey,
            testDir.EntityKey,
            snapshotFrequency,
            FixedClock.instance
        )

    let private auditUserId: EntityId<User> = EntityId<User>.Initialize None

    let private write (writer: IWriter, entity: ICounterEntity, event: IEvent) =
        writer.WriteAsync(
            entity.Id.Guid,
            entity.Rvn,
            auditUserId,
            event.EventJson,
            fun _ -> (entity.State :> IState).SnapshotJson
        )
        |> Async.RunSynchronously

    let private read (reader: IReader, guid) =
        reader.ReadAsync guid |> Async.RunSynchronously

    let private readAll (reader: IReader) =
        reader.ReadAllAsync() |> Async.RunSynchronously

    let private happyTests =
        testList "Happy tests" [
            testCase "WIP test"
            <| fun _ ->
                use testDir = new TestDirectory(None, nameof Counter)
                use agent = getAgents (testDir, Some 4u)

                let reader, writer = agent :> IReader, agent :> IWriter

                let result = result {
                    let counter, event = Counter.helper.InitializeFromCommand(Initialize -1)
                    let! _ = write (writer, counter, event)
                    let! counter, event = counter |> Counter.apply Increment
                    let! _ = write (writer, counter, event)
                    let! counter, event = counter |> Counter.apply Increment
                    let! _ = write (writer, counter, event)
                    let! counter, event = counter |> Counter.apply Decrement
                    let! _ = write (writer, counter, event)
                    let! counter, event = counter |> Counter.apply Increment
                    let! _ = write (writer, counter, event)
                    let! counter, event = counter |> Counter.apply Increment
                    let! _ = write (writer, counter, event)
                    let! counter, event = counter |> Counter.apply Decrement
                    let! _ = write (writer, counter, event)
                    let! counter, event = counter |> Counter.apply Increment
                    let! _ = write (writer, counter, event)

                    let guid = (counter :> ICounterEntity).Id.Guid
                    let! entriesForGuid = read (reader, guid)
                    let! actualForGuid = Counter.mapper.FromEntries(guid, entriesForGuid)

                    let! guidAndEntriesForOnly =
                        match readAll reader with
                        | [ result ] -> result
                        | [] -> Error "Reading all returned empty"
                        | _ -> Error "Reading all returned multiple"

                    let! actualForOnly = Counter.mapper.FromEntries guidAndEntriesForOnly

                    return actualForGuid, actualForOnly, counter
                }

                match result with
                | Ok(actualForGuid, actualForOnly, expected) ->
                    let expectedCount = 2

                    Expect.equal
                        (expected :> ICounterEntity).State.Count
                        expectedCount
                        $"Count for {nameof expected} should equal {expectedCount}"

                    Expect.equal actualForGuid expected $"{nameof actualForGuid} shoudl equal {nameof expected}"
                    Expect.equal actualForOnly expected $"{nameof actualForOnly} shoudl equal {nameof expected}"
                | Error _ -> Expect.isOk result $"{nameof result} should be Ok"
        ]

    (* TODO: Sad tests...
    let private sadTests =
        testList "Sad tests" [
            testCase "TEMP...Sad test"
            <| fun _ ->
                use _ = new TestDirectory(Some "Sad", nameof Counter, true)
                Expect.stringContains "Speedos" "paedo" "There's no paedo in Speedos"
        ]
    *)

    let tests = testList $"{nameof FileReaderAndWriter} tests" [ happyTests ]
