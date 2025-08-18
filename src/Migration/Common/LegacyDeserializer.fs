namespace Aornota.Ubersweep.Migration.Common

open Aornota.Ubersweep.Shared.Common

open Newtonsoft.Json

[<RequireQualifiedAccess>]
module LegacyDeserializer =

    let private jsonConverter = Fable.JsonConverter() :> JsonConverter

    let fromJson<'a> (Json json) =
        try
            Ok(JsonConvert.DeserializeObject<'a>(json, [| jsonConverter |]))
        with exn ->
            Error exn.Message
