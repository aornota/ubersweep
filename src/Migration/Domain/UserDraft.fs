namespace Aornota.Ubersweep.Migration.Domain

open System

type UserDraftId =
    | UserDraftId of guid: Guid

    static member Create() = Guid.NewGuid() |> UserDraftId
