namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Entities

[<AutoOpen>]
module Common =
    let fixedUtcNow = (FixedClock.instance :> IPersistenceClock).GetUtcNow()

    let auditUser1Id = UserId.Create()
    let auditUser2Id = UserId.Create()

    let sourceUser1 = User auditUser1Id
    let sourceUser2 = User auditUser2Id
