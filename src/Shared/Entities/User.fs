namespace Aornota.Ubersweep.Shared.Entities

type UserType =
    | SuperUser
    | Administrator
    | Pleb
    | PersonaNonGrata

type MustChangePasswordReason =
    | FirstSignIn
    | PasswordHasBeenReset

type UserId =
    private
    | UserId

    interface IId

type UserInitCommand = CreateUser of userName: string * password: string * userType: UserType

type UserCommand =
    | ChangeUserType of userType: UserType
    // TODO: Should this have "existing" password?...
    | ChangePassword of password: string * confirmPassword: string
    | ResetPassword of password: string * confirmPassword: string
