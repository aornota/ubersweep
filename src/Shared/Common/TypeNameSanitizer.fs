namespace Aornota.Ubersweep.Shared.Common

open System

[<AutoOpen>]
module TypeNameSanitizer =
    let sanitize (type': Type) = type'.Name.Split('`')[0]
