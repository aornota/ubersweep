namespace Aornota.Ubersweep.Shared.Entities

open System

type IId = interface end

type EntityId<'id when 'id :> IId> =
    private
    | Id of guid: Guid

    static member Create() = Id(Guid.NewGuid())
    static member FromGuid guid = Id guid

    member this.Guid =
        let (Id guid) = this
        guid
