namespace Aornota.Ubersweep.Tests.Server.Entities

open Aornota.Ubersweep.Shared.Entities
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Shared.Common

open Expecto
open FsToolkit.ErrorHandling
open System

[<RequireQualifiedAccess>]
module UserTests =
    let private happy =
        // TODO: Review these...
        testList "happy" [
            test "Initialize from command" {
                let guid, userName, userType = Guid.NewGuid(), "admin", Administrator

                let user, initEvent =
                    User.initializeFromCommand (guid, CreateUser(userName, "password", userType))

                Expect.equal user.Id.Guid guid $"Id.Guid for {user} should equal {guid}"

                Expect.equal user.Rvn Rvn.InitialRvn $"Rvn for {user} should equal {Rvn.InitialRvn}"

                Expect.equal user.State.UserName userName $"UserName for {user} should equal {userName}"

                Expect.equal user.State.UserType userType $"UserType for {user} should equal {userType}"

                // Cannot check specific Password[Salt|Hash] as these are non-deterministic.

                let expectedMustChangePasswordReason = Some FirstSignIn

                Expect.equal
                    user.State.MustChangePasswordReason
                    expectedMustChangePasswordReason
                    $"MustChangePasswordReason for {user} should equal {expectedMustChangePasswordReason}"
            }
            test "Initialize from event matches initialize from command" {
                let guid = Guid.NewGuid()

                let userFromCommand, initEvent =
                    User.initializeFromCommand (guid, CreateUser("pleb", "password", Pleb))

                let userFromEvent = User.eventHelper.InitializeFromEvent(guid, initEvent)

                Expect.equal userFromEvent userFromCommand $"{userFromEvent} should equal {userFromCommand}"
            }
            test "Change user type" {
                let guid, newUserType = Guid.NewGuid(), PersonaNonGrata

                let user, _ =
                    User.initializeFromCommand (guid, CreateUser("pleb", "password", Pleb))

                let rvn, initialState = user.Rvn, user.State

                let result = result {
                    let! user, _ = User.apply (ChangeUserType newUserType) user
                    return user
                }

                match result with
                | Ok user ->
                    Expect.equal user.Id.Guid guid $"Id.Guid for {user} should equal {guid}"
                    Expect.equal user.Rvn rvn.NextRvn $"Rvn for {user} should equal {rvn.NextRvn}"
                    Expect.equal user.State.UserType newUserType $"State.UserType for {user} should equal {newUserType}"

                    Expect.equal
                        user.State.UserName
                        initialState.UserName
                        $"State.UserName should be equal for {user} and {initialState}"

                    Expect.equal
                        user.State.PasswordSalt
                        initialState.PasswordSalt
                        $"State.PasswordSalt should be equal for {user} and {initialState}"

                    Expect.equal
                        user.State.PasswordHash
                        initialState.PasswordHash
                        $"State.PasswordHash should be equal for {user} and {initialState}"

                    Expect.equal
                        user.State.MustChangePasswordReason
                        initialState.MustChangePasswordReason
                        $"State.MustChangePasswordReason should be equal for {user} and {initialState}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
        // TODO: Change password...
        // TODO: Reset password...
        ]

    let tests = testList $"{nameof User}" [ happy ]
