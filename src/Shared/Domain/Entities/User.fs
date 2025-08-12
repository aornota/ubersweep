namespace Aornota.Ubersweep.Shared.Domain.Entities

open Aornota.Ubersweep.Shared

type UserType =
    | SuperUser
    | Administrator
    | Pleb
    | PersonaNonGrata

type MustChangePasswordReason =
    | FirstSignIn
    | PasswordHasBeenReset

type UserInitCommand = Create of userName: string * password: string * userType: UserType

type UserCommand =
    | ChangeUserType of userType: UserType
    // TODO: Should this have "existing" password?...
    | ChangePassword of password: string * confirmPassword: string
    | ResetPassword of password: string * confirmPassword: string

type User = {
    UserName: string
    PasswordSalt: string
    PasswordHash: string
    UserType: UserType
    MustChangePasswordReason: MustChangePasswordReason option
} with

    interface IEntity with
        member this.SnapshotJson = Json.toJson this
