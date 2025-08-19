namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type UserDraftId =
    private
    | UserDraftId of guid: Guid

    static member Create() = UserDraftId(Guid.NewGuid())
    static member FromGuid guid = UserDraftId guid

    interface IId with
        member this.Guid =
            let (UserDraftId guid) = this
            guid
