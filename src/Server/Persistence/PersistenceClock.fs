namespace Aornota.Ubersweep.Server.Persistence

open System

type PersistenceClock() =
    interface IPersistenceClock with
        member _.GetUtcNow() = DateTime.UtcNow
