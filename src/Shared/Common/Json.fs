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
    let spaceCount = 0 // note: need compact serialization because persistence requires that each serialized event is a single line

    let caseStrategy = PascalCase

    let extraCoders = // note: needed to handle Decimal - and (for some reason) unit
        Extra.empty
        |> Extra.withDecimal
        |> Extra.withCustom (fun _ -> Encode.nil) (fun _ _ -> Ok())

#if FABLE_COMPILER
    let inline encode<'a> value =
        Json(Encode.Auto.toString<'a> (spaceCount, value, caseStrategy, extraCoders))

    let inline fromJson<'a> (Json json) =
        decode.Auto.fromString<'a> (json, caseStrategy, extraCoders)
#else
    let encode<'a> value =
        Json(Encode.Auto.toString<'a> (spaceCount, value, caseStrategy, extraCoders))

    let decode<'a> (Json json) =
        Decode.Auto.fromString<'a> (json, caseStrategy, extraCoders)
#endif
