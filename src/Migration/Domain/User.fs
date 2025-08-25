namespace Aornota.Ubersweep.Migration.Domain

open System

type UserId' =
    | UserId of guid: Guid

    static member Create() = Guid.NewGuid() |> UserId
