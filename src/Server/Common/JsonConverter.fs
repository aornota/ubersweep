module Aornota.Ubersweep.Server.Common.JsonConverter

open Aornota.Ubersweep.Shared.Json

open Thoth.Json.Net

let private extraCoders = // note: needed to handle unit (for some reason)
    Extra.empty
    |> Extra.withDecimal
    |> Extra.withCustom (fun _ -> Encode.nil) (fun _ _ -> Ok())

// Note: toJson/fromJson differ from Aornota.Ubersweep.Client versions; see notes for latter for details.

let toJson<'a> value =
    Json(Encode.Auto.toString<'a> (SpaceCount, value, extra = extraCoders))

let fromJson<'a> (Json json) =
    Decode.Auto.fromString<'a> (json, extra = extraCoders)
