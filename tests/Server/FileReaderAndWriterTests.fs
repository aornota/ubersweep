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

    let private write (writer: IWriter, counter: Counter, event: CounterEvent) =
        writer.WriteAsync(
            (counter :> ICounterEntity).Id.Guid,
            (counter :> ICounterEntity).Rvn,
            auditUserId,
            (event :> IEvent).EventJson,
            fun _ -> ((counter :> ICounterEntity).State :> IState).SnapshotJson
        )
        |> Async.RunSynchronously

    let private read (reader: IReader, counter: Counter) =
        reader.ReadAsync (counter :> ICounterEntity).Id.Guid |> Async.RunSynchronously

    let private happyTests =
        testList "Happy tests" [
            testCase "TEMP...Happy test"
            <| fun _ ->
                use testDir = new TestDirectory(None, nameof Counter)
                use agent = getAgents (testDir, None)

                let reader, writer = agent :> IReader, agent :> IWriter
                let guid = Guid.NewGuid()
                let counter = Counter.helper.Initialize(Some guid)

                // TODO: Test is failing wotj "File does not exist when writing non-initial (Rvn 2u)" because initialized counter is Rvn 1 - so need to rethink this...

                let result = result {
                    let! counter, event = counter |> Counter.apply Increment
                    let! _ = write (writer, counter, event)
                    let! counter, event = counter |> Counter.apply Increment
                    let! _ = write (writer, counter, event)

                    let! entries = read (reader, counter)

                    return! Counter.mapper.FromEntries((counter :> ICounterEntity).Id.Guid, entries)
                }

                Expect.isOk result $"{nameof result} should be Ok"
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
