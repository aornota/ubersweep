module Aornota.Ubersweep.Shared.NonEmptyString

open System

type NonEmptyString = private {
    String': string
} with

    static member TryCreate string =
        if not (String.IsNullOrWhiteSpace string) then
            Ok { String' = string }
        else
            Error $"{nameof string} is null, empty, or whitespace-only"

    member this.String = this.String'
