namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open Thoth.Json.Net

// TODO-ENTITIES: Think about permissions...

type UserInitEvent =
    | UserCreated of userName: string * passwordSalt: string * passwordHash: string * userType: UserType

    interface IEvent with
        member this.EventJson = Json.encode this

type UserEvent =
    | UserTypeChanged of userType: UserType
    | PasswordChanged of passwordSalt: string * passwordHash: string
    | PasswordReset of passwordSalt: string * passwordHash: string

    interface IEvent with
        member this.EventJson = Json.encode this

type User = {
    UserCommon: UserCommon'
    PasswordSalt: string
    PasswordHash: string
} with

    interface IState<User, UserEvent> with
        member this.SnapshotJson = Json.encode this

        member this.Evolve event =
            match event with
            | UserTypeChanged userType ->
                Ok {
                    this with
                        UserCommon.UserType = userType
                }
            | PasswordChanged(passwordSalt, passwordHash) ->
                Ok {
                    this with
                        UserCommon.MustChangePasswordReason = None
                        PasswordSalt = passwordSalt
                        PasswordHash = passwordHash
                }
            | PasswordReset(passwordSalt, passwordHash) ->
                Ok {
                    this with
                        UserCommon.MustChangePasswordReason = Some MustChangePasswordReason.PasswordReset
                        PasswordSalt = passwordSalt
                        PasswordHash = passwordHash
                }

type UserHelper() =
    inherit EntityHelper<UserId, User, UserInitCommand, UserInitEvent, UserEvent>()

    let eventDecoder =
        Decode.Auto.generateDecoderCached<UserEvent> (Json.caseStrategy, Json.extraCoders)

    let initEventDecoder =
        Decode.Auto.generateDecoderCached<UserInitEvent> (Json.caseStrategy, Json.extraCoders)

    let stateDecoder =
        Decode.Auto.generateDecoderCached<User> (Json.caseStrategy, Json.extraCoders)

    override _.DecodeEvent(Json json) = Decode.fromString eventDecoder json
    override _.DecodeInitEvent(Json json) = Decode.fromString initEventDecoder json
    override _.DecodeState(Json json) = Decode.fromString stateDecoder json
    override _.IdFromGuid guid = UserId.FromGuid guid

    override _.InitFromCommand(guid, CreateUser(userName, password, userType)) =
        let passwordSalt = salt ()
        let passwordHash = hash (password, passwordSalt)

        {
            Id = UserId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = {
                UserCommon = {
                    UserName = userName
                    UserType = userType
                    MustChangePasswordReason = Some FirstSignIn
                }
                PasswordSalt = passwordSalt
                PasswordHash = passwordHash
            }
        },
        UserCreated(userName, passwordSalt, passwordHash, userType)

    override _.InitFromEvent(guid, UserCreated(userName, passwordSalt, passwordHash, userType)) = {
        Id = UserId.FromGuid guid
        Rvn = Rvn.InitialRvn
        State = {
            UserCommon = {
                UserName = userName
                UserType = userType
                MustChangePasswordReason = Some FirstSignIn
            }
            PasswordSalt = passwordSalt
            PasswordHash = passwordHash
        }
    }

[<RequireQualifiedAccess>]
module User =
    let private decide command (_: User) =
        match command with
        | ChangeUserType userType -> Ok(UserTypeChanged userType)
        | ChangePassword(password, confirmationPassword) ->
            if confirmationPassword <> password then
                Error "Confirmation password does not match password"
            else
                let passwordSalt = salt ()
                Ok(PasswordChanged(passwordSalt, hash (password, passwordSalt)))
        | ResetPassword(password, confirmationPassword) ->
            if confirmationPassword <> password then
                Error "Confirmation password does not match password"
            else
                let passwordSalt = salt ()
                Ok(PasswordReset(passwordSalt, hash (password, passwordSalt)))

    let helper = UserHelper()

    let apply command (entity: Entity<UserId, User, UserEvent>) = result {
        let! event = decide command entity.State
        let! entity = entity.Evolve event
        return entity, event
    }
