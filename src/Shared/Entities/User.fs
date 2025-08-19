namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type UserType =
    | SuperUser
    | Administrator
    | Pleb
    | PersonaNonGrata

type MustChangePasswordReason =
    | FirstSignIn
    | PasswordReset

type UserId =
    private
    | UserId of guid: Guid

    static member Create() = UserId(Guid.NewGuid())
    static member FromGuid guid = UserId guid

    interface IId with
        member this.Guid =
            let (UserId guid) = this
            guid

type UserInitCommand = CreateUser of userName: string * password: string * userType: UserType

type UserCommand =
    | ChangeUserType of userType: UserType
    // TODO: Should this have "existing" password?...
    | ChangePassword of password: string * confirmPassword: string
    | ResetPassword of password: string * confirmPassword: string

type UserCommon' = {
    UserName: string
    UserType: UserType
    MustChangePasswordReason: MustChangePasswordReason option
}
