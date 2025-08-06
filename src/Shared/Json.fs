module Aornota.Ubersweep.Shared.Json

type Json = Json of json: string

[<Literal>]
let SpaceCount = 0 // note: need compact serialization because persistence requires that each serialized event is a single line
