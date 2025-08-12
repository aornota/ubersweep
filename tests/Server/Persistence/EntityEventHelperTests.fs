namespace Aornota.Ubersweep.Tests.Server.Persistence

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain.Entities
open Aornota.Ubersweep.Tests.Server.Common

open Expecto
open FsToolkit.ErrorHandling
open System

[<RequireQualifiedAccess>]
module EntityEventHelperTests =
    let private happy =
        testList "happy" [
            test "From single event entry" {
                let helper = CounterEventHelper()
                let guid = Guid.NewGuid()

                let result = result {
                    let entries =
                        NonEmptyList<Entry>
                            .Create(
                                EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                []
                            )

                    return! helper.FromEntries(guid, entries)
                }

                match result with
                | Ok counter ->
                    let expectedCounter =
                        Entity<Counter>(EntityId<Counter>.FromGuid guid, Rvn.InitialRvn, { Count = -1 })

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
                    let expectedCounter =
                        Entity<Counter>(EntityId<Counter>.FromGuid guid, Rvn 3u, { Count = 1 })

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
                                SnapshotJson(Rvn 3u, ({ Count = 1 } :> IEntity).SnapshotJson),
                                [
                                    EventJson(Rvn 4u, fixedUtcNow, auditUser1Id, (Incremented :> IEvent).EventJson)
                                ]
                            )

                    return! helper.FromEntries(guid, entries)
                }

                match result with
                | Ok counter ->
                    let expectedCounter =
                        Entity<Counter>(EntityId<Counter>.FromGuid guid, Rvn 4u, { Count = 2 })

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
                                    SnapshotJson(Rvn 3u, ({ Count = 1 } :> IEntity).SnapshotJson)
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

    let tests = testList $"{nameof EntityEventHelper}" [ happy; sad ]
