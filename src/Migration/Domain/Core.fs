namespace Aornota.Ubersweep.Migration.Domain

type IGroup = interface end

type Group4 =
    | GroupA
    | GroupB
    | GroupC
    | GroupD

    interface IGroup

type Group6 =
    | GroupA
    | GroupB
    | GroupC
    | GroupD
    | GroupE
    | GroupF

    interface IGroup

type Group8 =
    | GroupA
    | GroupB
    | GroupC
    | GroupD
    | GroupE
    | GroupF
    | GroupG
    | GroupH

    interface IGroup

type DraftOrdinal = DraftOrdinal of draftOrdinal: int
