namespace Aornota.Ubersweep.Tests.Server.Persistence

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Tests.Server.Common

open Expecto
open FsToolkit.ErrorHandling
open System

[<RequireQualifiedAccess>]
module EntityHelperTests =
    let private happy =
        testList "happy" [
            test "From single event entry" {
                let helper = CounterEventHelper()
                let guid = Guid.NewGuid()

                let result = result {
                    let entries =
                        NonEmptyList<Entry>
                            .Create(
                                EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized 1 :> IEvent).EventJson),
                                []
                            )

                    return! helper.FromEntries(guid, entries)
                }

                match result with
                | Ok counter ->
                    let expectedCounter = {
                        Id = CounterId.FromGuid guid
                        Rvn = Rvn.InitialRvn
                        State = { Count = 1 }
                    }

                    Expect.equal counter expectedCounter $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            test "From multiple event entries" {
                let helper = CounterEventHelper()
                let guid = Guid.NewGuid()

                let result = result {
                    let entries =
                        NonEmptyList<Entry>
                            .Create(
                                EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                [
                                    EventJson(Rvn 2u, fixedUtcNow, auditUser1Id, (Incremented :> IEvent).EventJson)
                                    EventJson(Rvn 3u, fixedUtcNow, auditUser2Id, (Incremented :> IEvent).EventJson)
                                ]
                            )

                    return! helper.FromEntries(guid, entries)
                }

                match result with
                | Ok counter ->
                    let expectedCounter = {
                        Id = CounterId.FromGuid guid
                        Rvn = Rvn 3u
                        State = { Count = 1 }
                    }

                    Expect.equal counter expectedCounter $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            test "From multiple entries with snapshot" {
                let helper = CounterEventHelper()
                let guid = Guid.NewGuid()

                let result = result {
                    let entries =
                        NonEmptyList<Entry>
                            .Create(
                                SnapshotJson(Rvn 3u, ({ Count = 1 } :> IState<Counter, CounterEvent>).SnapshotJson),
                                [
                                    EventJson(Rvn 4u, fixedUtcNow, auditUser1Id, (Incremented :> IEvent).EventJson)
                                ]
                            )

                    return! helper.FromEntries(guid, entries)
                }

                match result with
                | Ok counter ->
                    let expectedCounter = {
                        Id = CounterId.FromGuid guid
                        Rvn = Rvn 4u
                        State = { Count = 2 }
                    }

                    Expect.equal counter expectedCounter $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
        ]

    let private sad =
        testList "sad" [
            test "From entries when subsequent entries contain snapshot" {
                let helper = CounterEventHelper()
                let guid = Guid.NewGuid()

                let result = result {
                    let entries =
                        NonEmptyList<Entry>
                            .Create(
                                EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                [
                                    EventJson(Rvn 2u, fixedUtcNow, auditUser1Id, (Incremented :> IEvent).EventJson)
                                    EventJson(Rvn 3u, fixedUtcNow, auditUser2Id, (Incremented :> IEvent).EventJson)
                                    SnapshotJson(Rvn 3u, ({ Count = 1 } :> IState<Counter, CounterEvent>).SnapshotJson)
                                ]
                            )

                    return! helper.FromEntries(guid, entries)
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"Subsequent entries contain a {nameof SnapshotJson}"
                        $"{nameof Error} is not the expected error"
            }
        ]

    let tests = testList $"{nameof EntityHelper}" [ happy; sad ]
