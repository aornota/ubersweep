namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Domain.Entities

[<AutoOpen>]
module Common =
    let fixedUtcNow = (FixedClock.instance :> IPersistenceClock).GetUtcNow()

    let auditUser1Id: EntityId<User> = EntityId<User>.Create()
    let auditUser2Id: EntityId<User> = EntityId<User>.Create()
