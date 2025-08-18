namespace Aornota.Ubersweep.Migration.Domain

open System

type UserId =
    | UserId of guid: Guid

    static member Create() = Guid.NewGuid() |> UserId

type UserType =
    | SuperUser
    | Administrator
    | Pleb
    | PersonaNonGrata

type MustChangePasswordReason =
    | FirstSignIn
    | PasswordReset
