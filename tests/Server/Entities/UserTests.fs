namespace Aornota.Ubersweep.Tests.Server.Entities

open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Shared.Entities
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Tests.Server.Common

open Expecto
open FsToolkit.ErrorHandling
open System

[<RequireQualifiedAccess>]
module UserTests =
    let private happy =
        testList "happy" [
            test "Initialize from command" {
                let guid, userName, userType = Guid.NewGuid(), "admin", Administrator

                let user, initEvent =
                    User.helper.InitFromCommand(guid, CreateUser(userName, "password", userType))

                // Note: Cannot check specific Password[Salt|Hash] as these are non-deterministic.
                user.Guid |> Check.equal guid
                user.Rvn |> Check.equal Rvn.InitialRvn
                user.State.UserCommon.UserName |> Check.equal userName
                user.State.UserCommon.UserType |> Check.equal userType
                user.State.UserCommon.MustChangePasswordReason |> Check.equal (Some FirstSignIn)
            }
            test "Initialize from event matches initialize from command" {
                let guid = Guid.NewGuid()

                let userFromCommand, initEvent =
                    User.helper.InitFromCommand(guid, CreateUser("pleb", "password", Pleb))

                let userFromEvent = User.helper.InitFromEvent(guid, initEvent)

                userFromEvent |> Check.equal userFromCommand
            }
            test "Change user type" {
                let guid, newUserType = Guid.NewGuid(), PersonaNonGrata

                let user, _ =
                    User.helper.InitFromCommand(guid, CreateUser("pleb", "password", Pleb))

                let result = result { return! User.apply (ChangeUserType newUserType) user }

                result
                |> Check.isOk (
                    {
                        Id = user.Id
                        Rvn = user.Rvn.NextRvn
                        State = {
                            user.State with
                                UserCommon.UserType = newUserType
                        }
                    },
                    UserTypeChanged newUserType
                )
            }
            test "Change password" {
                let guid = Guid.NewGuid()

                let user, _ =
                    User.helper.InitFromCommand(guid, CreateUser("pleb", "password", Pleb))

                let result = result { return! User.apply (ChangePassword("123456", "123456")) user }

                match result with
                | Ok(updatedUser, event) ->
                    updatedUser.Guid |> Check.equal user.Guid
                    updatedUser.Rvn |> Check.equal user.Rvn.NextRvn

                    updatedUser.State.UserCommon.UserName
                    |> Check.equal user.State.UserCommon.UserName

                    updatedUser.State.UserCommon.UserType
                    |> Check.equal user.State.UserCommon.UserType

                    updatedUser.State.UserCommon.MustChangePasswordReason |> Check.equal None
                    updatedUser.State.PasswordSalt |> Check.notEqual user.State.PasswordSalt
                    updatedUser.State.PasswordHash |> Check.notEqual user.State.PasswordHash

                    match event with
                    | PasswordChanged _ -> // note: cannot check specific Password[Salt|Hash] as these are non-deterministic
                        ()
                    | _ ->
                        // TODO-TESTS: Rethink how to force test failure...
                        failwith $"{nameof UserEvent} expected to be {nameof PasswordChanged} but is {event}"
                | Error error ->
                    // TODO-TESTS: Rethink how to force test failure...
                    failwith $"Unexpected error: {error}"
            }
            test "Reset password" {
                let guid = Guid.NewGuid()

                let user, _ =
                    User.helper.InitFromCommand(guid, CreateUser("pleb", "password", Pleb))

                let result = result { return! User.apply (ResetPassword("123456", "123456")) user }

                match result with
                | Ok(updatedUser, event) ->
                    updatedUser.Guid |> Check.equal user.Guid
                    updatedUser.Rvn |> Check.equal user.Rvn.NextRvn

                    updatedUser.State.UserCommon.UserName
                    |> Check.equal user.State.UserCommon.UserName

                    updatedUser.State.UserCommon.UserType
                    |> Check.equal user.State.UserCommon.UserType

                    updatedUser.State.UserCommon.MustChangePasswordReason
                    |> Check.equal (Some PasswordReset)

                    updatedUser.State.PasswordSalt |> Check.notEqual user.State.PasswordSalt
                    updatedUser.State.PasswordHash |> Check.notEqual user.State.PasswordHash

                    match event with
                    | UserEvent.PasswordReset _ -> // note: cannot check specific Password[Salt|Hash] as these are non-deterministic
                        ()
                    | _ ->
                        // TODO-TESTS: Rethink how to force test failure...
                        failwith $"{nameof UserEvent} expected to be {nameof UserEvent.PasswordReset} but is {event}"
                | Error error ->
                    // TODO-TESTS: Rethink how to force test failure...
                    failwith $"Unexpected error: {error}"
            }
        ]

    let tests = testList $"{nameof User}" [ happy ]
