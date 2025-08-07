namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence

open System

type FixedClock() =
    let fixedUtcNow = DateTime(2025, 08, 07, 15, 11, 33, DateTimeKind.Utc)

    interface IPersistenceClock with
        member _.GetUtcNow() = fixedUtcNow

module FixedClock =
    let instance = FixedClock()
