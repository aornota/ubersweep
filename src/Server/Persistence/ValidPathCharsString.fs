module Aornota.Ubersweep.Server.Persistence.ValidPathCharsString

open Aornota.Ubersweep.Shared.NonEmptyString

open System.IO

let private invalidPathChars = Path.GetInvalidPathChars()

type ValidPathCharsString = private {
    String': string
} with

    static member TryCreate string =
        match NonEmptyString.TryCreate string with
        | Ok _ ->
            match
                string
                |> List.ofSeq
                |> List.exists (fun char -> invalidPathChars |> Array.contains char)
            with
            | false -> Ok { String' = string }
            | true -> Error $"{nameof string} contains invalid path characters"
        | Error error -> Error error

    member this.String = this.String'
