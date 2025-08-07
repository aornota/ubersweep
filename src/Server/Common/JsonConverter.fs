namespace Aornota.Ubersweep.Server.Common

open Aornota.Ubersweep.Shared

open Thoth.Json.Net

module JsonConverter =
    let private extraCoders = // note: needed to handle unit (for some reason)
        Extra.empty
        |> Extra.withDecimal
        |> Extra.withCustom (fun _ -> Encode.nil) (fun _ _ -> Ok())

    // Note: toJson/fromJson differ from Aornota.Ubersweep.Client versions; see notes for latter for details.

    let toJson<'a> value =
        Json(Encode.Auto.toString<'a> (Json.SpaceCount, value, extra = extraCoders))

    let fromJson<'a> (Json json) =
        Decode.Auto.fromString<'a> (json, extra = extraCoders)
