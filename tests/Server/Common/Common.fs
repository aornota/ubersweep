namespace Aornota.Ubersweep.Tests.Server.Common

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Entities

[<AutoOpen>]
module Common =
    let fixedUtcNow = (FixedClock.instance :> IPersistenceClock).GetUtcNow()

    let auditUser1Id: EntityId<UserId> = EntityId<UserId>.Create()
    let auditUser2Id: EntityId<UserId> = EntityId<UserId>.Create()
