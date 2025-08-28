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
        // TODO-TESTS: Review these...
        testList "happy" [
            test "Initialize from command" {
                let guid, userName, userType = Guid.NewGuid(), "admin", Administrator

                let user, initEvent =
                    User.helper.InitFromCommand(guid, CreateUser(userName, "password", userType))

                user.Guid |> Check.equal guid
                user.Rvn |> Check.equal Rvn.InitialRvn
                user.State.UserCommon.UserName |> Check.equal userName
                user.State.UserCommon.UserType |> Check.equal userType

                // Cannot check specific Password[Salt|Hash] as these are non-deterministic.

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

                let result = result {
                    let! user, _ = User.apply (ChangeUserType newUserType) user
                    return user
                }

                result
                |> Check.isOk {
                    Id = user.Id
                    Rvn = user.Rvn.NextRvn
                    State = {
                        user.State with
                            UserCommon.UserType = newUserType
                    }
                }
            }
        // TODO-TESTS: Change password...
        // TODO-TESTS: Reset password...
        ]

    let tests = testList $"{nameof User}" [ happy ]
