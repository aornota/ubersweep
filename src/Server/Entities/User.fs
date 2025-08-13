namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling

// TODO: Think about permissions...

type User = {
    UserName: string
    PasswordSalt: string
    PasswordHash: string
    UserType: UserType
    MustChangePasswordReason: MustChangePasswordReason option
} with

    interface IEntity with
        member this.SnapshotJson = Json.toJson this

type UserInitEvent =
    | UserCreated of userName: string * passwordSalt: string * passwordHash: string * userType: UserType

    interface IEvent with
        member this.EventJson = Json.toJson this

type UserEvent =
    | UserTypeChanged of userType: UserType
    | PasswordChanged of passwordSalt: string * passwordHash: string
    | PasswordReset of passwordSalt: string * passwordHash: string

    interface IEvent with
        member this.EventJson = Json.toJson this

type UserEventHelper() =
    inherit EntityEventHelper<UserId, User, UserInitEvent, UserEvent>()

    override _.InitializeFromEvent(guid, UserCreated(userName, passwordSalt, passwordHash, userType)) =
        Entity<UserId, User>(
            EntityId<UserId>.FromGuid guid,
            Rvn.InitialRvn,
            {
                UserName = userName
                PasswordSalt = passwordSalt
                PasswordHash = passwordHash
                UserType = userType
                MustChangePasswordReason = Some FirstSignIn
            }
        )

    override _.Evolve entity event =
        let state =
            match event with
            | UserTypeChanged userType -> {
                entity.State with
                    UserType = userType
              }
            | PasswordChanged(passwordSalt, passwordHash) -> {
                entity.State with
                    PasswordSalt = passwordSalt
                    PasswordHash = passwordHash
                    MustChangePasswordReason = None
              }
            | PasswordReset(passwordSalt, passwordHash) -> {
                entity.State with
                    PasswordSalt = passwordSalt
                    PasswordHash = passwordHash
                    MustChangePasswordReason = Some PasswordHasBeenReset
              }

        entity.Evolve state
        entity

[<RequireQualifiedAccess>]
module User =
    let private decide command (_: Entity<UserId, User>) =
        match command with
        | ChangeUserType userType -> Ok(UserTypeChanged userType)
        | ChangePassword(password, confirmPassword) ->
            if confirmPassword <> password then
                Error "TODO..."
            else
                let passwordSalt = salt ()
                Ok(PasswordChanged(passwordSalt, hash (password, passwordSalt)))
        | ResetPassword(password, confirmPassword) ->
            if confirmPassword <> password then
                Error "TODO..."
            else
                let passwordSalt = salt ()
                Ok(PasswordReset(passwordSalt, hash (password, passwordSalt)))

    let eventHelper = UserEventHelper()

    let initializeFromCommand (guid, CreateUser(userName, password, userType)) =
        let passwordSalt = salt ()
        let passwordHash = hash (password, passwordSalt)

        Entity<UserId, User>(
            EntityId<UserId>.FromGuid guid,
            Rvn.InitialRvn,
            {
                UserName = userName
                PasswordSalt = passwordSalt
                PasswordHash = passwordHash
                UserType = userType
                MustChangePasswordReason = Some FirstSignIn
            }
        ),
        UserCreated(userName, passwordSalt, passwordHash, userType)

    let apply command entity = result {
        let! event = decide command entity
        return eventHelper.Evolve entity event, event
    }
