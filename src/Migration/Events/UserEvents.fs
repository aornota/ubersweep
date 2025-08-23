namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities
open Aornota.Ubersweep.Migration.Domain // after Aornota.Ubersweep.Shared.Entities to ensure migration types used when exist in both

type UserName = UserName of userName: string
type Salt = Salt of string
type Hash = Hash of string

type UserEvent =
    | UserCreated of userId: UserId * userName: UserName * passwordSalt: Salt * passwordHash: Hash * userType: UserType
    | PasswordChanged of userId: UserId * passwordSalt: Salt * passwordHash: Hash
    | PasswordReset of userId: UserId * passwordSalt: Salt * passwordHash: Hash
    | UserTypeChanged of userId: UserId * userType: UserType

type User = {
    UserName: string
    UserType: UserType
    MustChangePasswordReason: MustChangePasswordReason option
    PasswordSalt: string
    PasswordHash: string
}

type UserHelper() =
    let rec applyEvents events userAndRvn =
        match userAndRvn, events with
        | None, UserCreated(_, UserName userName, Salt passwordSalt, Hash passwordHash, userType) :: t ->
            applyEvents
                t
                (Some(
                    {
                        UserName = userName
                        PasswordSalt = passwordSalt
                        PasswordHash = passwordHash
                        UserType = userType
                        MustChangePasswordReason = Some FirstSignIn
                    },
                    Rvn.InitialRvn
                ))
        | Some(user, rvn), PasswordChanged(_, Salt passwordSalt, Hash passwordHash) :: t ->
            applyEvents
                t
                (Some(
                    {
                        user with
                            PasswordSalt = passwordSalt
                            PasswordHash = passwordHash
                            MustChangePasswordReason = None
                    },
                    rvn.NextRvn
                ))
        | Some(user, rvn), PasswordReset(_, Salt passwordSalt, Hash passwordHash) :: t ->
            applyEvents
                t
                (Some(
                    {
                        user with
                            PasswordSalt = passwordSalt
                            PasswordHash = passwordHash
                            MustChangePasswordReason =
                                match user.MustChangePasswordReason with
                                | Some FirstSignIn -> Some FirstSignIn
                                | Some MustChangePasswordReason.PasswordReset
                                | None -> Some MustChangePasswordReason.PasswordReset
                    },
                    rvn.NextRvn
                ))
        | Some(user, rvn), UserTypeChanged(_, userType) :: t ->
            applyEvents t (Some({ user with UserType = userType }, rvn.NextRvn))
        | Some userAndRvn, [] -> Ok userAndRvn
        | None, [] -> Error $"No initial {nameof UserEvent}"
        | None, h :: _ -> Error $"Invalid initial {nameof UserEvent}: {h}"
        | Some _, UserCreated _ :: _ -> Error $"Invalid non-initial {nameof UserEvent}: {nameof UserCreated}"

    interface IHelper<UserEvent, User> with
        member _.ApplyEvents events = applyEvents events None
