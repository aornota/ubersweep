namespace Aornota.Ubersweep.Tests.Server.Entities

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
                                EventJson(Rvn 1u, fixedUtcNow, sourceUser1, (Initialized 1 :> IEvent).EventJson),
                                []
                            )

                    return! helper.FromEntries(guid, entries)
                }

                result
                |> Check.isOk (
                    Ok {
                        Id = CounterId.FromGuid guid
                        Rvn = Rvn.InitialRvn
                        State = { Count = 1 }
                    }
                )
            }
            test "From multiple event entries" {
                let helper = CounterEventHelper()
                let guid = Guid.NewGuid()

                let result = result {
                    let entries =
                        NonEmptyList<Entry>
                            .Create(
                                EventJson(Rvn 1u, fixedUtcNow, sourceUser1, (Initialized -1 :> IEvent).EventJson),
                                [
                                    EventJson(Rvn 2u, fixedUtcNow, sourceUser1, (Incremented :> IEvent).EventJson)
                                    EventJson(Rvn 3u, fixedUtcNow, sourceUser2, (Incremented :> IEvent).EventJson)
                                ]
                            )

                    return! helper.FromEntries(guid, entries)
                }

                result
                |> Check.isOk (
                    Ok {
                        Id = CounterId.FromGuid guid
                        Rvn = Rvn 3u
                        State = { Count = 1 }
                    }
                )
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
                                    EventJson(Rvn 4u, fixedUtcNow, sourceUser1, (Incremented :> IEvent).EventJson)
                                ]
                            )

                    return! helper.FromEntries(guid, entries)
                }

                result
                |> Check.isOk (
                    Ok {
                        Id = CounterId.FromGuid guid
                        Rvn = Rvn 4u
                        State = { Count = 2 }
                    }
                )
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
                                EventJson(Rvn 1u, fixedUtcNow, sourceUser1, (Initialized -1 :> IEvent).EventJson),
                                [
                                    EventJson(Rvn 2u, fixedUtcNow, sourceUser1, (Incremented :> IEvent).EventJson)
                                    EventJson(Rvn 3u, fixedUtcNow, sourceUser2, (Incremented :> IEvent).EventJson)
                                    SnapshotJson(Rvn 3u, ({ Count = 1 } :> IState<Counter, CounterEvent>).SnapshotJson)
                                ]
                            )

                    return! helper.FromEntries(guid, entries)
                }

                result |> Check.isError $"Subsequent entries contain a {nameof SnapshotJson}"
            }
        ]

    let tests = testList $"{nameof EntityHelper}" [ happy; sad ]
