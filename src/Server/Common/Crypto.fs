namespace Aornota.Ubersweep.Server.Common

open System
open System.Security.Cryptography
open System.Text

[<AutoOpen>]
module Crypto =
    let private rng = RandomNumberGenerator.Create()
    let private sha512 = SHA512.Create()
    let private encoding = Encoding.UTF8

    let salt () =
        let bytes: byte[] = Array.zeroCreate 32
        rng.GetBytes bytes
        Convert.ToBase64String bytes

    let hash (password, salt) =
        let bytes = sha512.ComputeHash(encoding.GetBytes $"{password}{salt}") // password is therefore case-sensitive
        Convert.ToBase64String bytes
