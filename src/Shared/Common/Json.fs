namespace Aornota.Ubersweep.Shared.Common

type Json = Json of json: string

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

[<RequireQualifiedAccess>]
module Json =
    [<Literal>]
    let private SpaceCount = 0 // note: need compact serialization because persistence requires that each serialized event is a single line

    let private extraCoders = // note: needed to handle unit (for some reason)
        Extra.empty
        |> Extra.withDecimal
        |> Extra.withCustom (fun _ -> Encode.nil) (fun _ _ -> Ok())

#if FABLE_COMPILER
    let inline toJson<'a> value =
        Json(Encode.Auto.toString<'a> (SpaceCount, value, extra = extraCoders))

    let inline fromJson<'a> (Json json) =
        Decode.Auto.fromString<'a> (json, extra = extraCoders)
#else
    let toJson<'a> value =
        Json(Encode.Auto.toString<'a> (SpaceCount, value, extra = extraCoders))

    let fromJson<'a> (Json json) =
        Decode.Auto.fromString<'a> (json, extra = extraCoders)
#endif
