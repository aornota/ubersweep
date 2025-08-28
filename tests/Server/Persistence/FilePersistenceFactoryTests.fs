namespace Aornota.Ubersweep.Tests.Server.Persistence

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Tests.Server.Common

open Expecto
open FsToolkit.ErrorHandling
open System
open System.IO

// TODO-TESTS...type private TestPersistenceDir

[<RequireQualifiedAccess>]
module FilePersistenceFactoryTests =
    let private happy =
        testList "happy" [
            testList "ReadAllAsync" [
            (* TODO-TESTS:
            *)
            ]
            testList "CreateFromSnapshotAsync" [
            (* TODO-TESTS:
            *)
            ]
            testList "WriteEventAsync" [
            (* TODO-TESTS:
            *)
            ]
        ]

    let private sad =
        testList "sad" [
            testList "ReadAllAsync" [
            (* TODO-TESTS:
            *)
            ]
            testList "CreateFromSnapshotAsync" [
            (* TODO-TESTS:
            *)
            ]
            testList "WriteEventAsync" [
            (* TODO-TESTS:
            *)
            ]
        ]

    let tests = testList $"{nameof FilePersistenceFactory}" [ happy; sad ]
