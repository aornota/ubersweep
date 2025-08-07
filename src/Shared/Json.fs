namespace Aornota.Ubersweep.Shared

type Json = Json of json: string

[<RequireQualifiedAccess>]
module Json =
    [<Literal>]
    let SpaceCount = 0 // note: need compact serialization because persistence requires that each serialized event is a single line
