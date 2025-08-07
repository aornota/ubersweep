namespace Aornota.Ubersweep.Shared.Domain

open System

type UserId =
    | UserId of guid: Guid

    static member Create() = Guid.NewGuid() |> UserId
