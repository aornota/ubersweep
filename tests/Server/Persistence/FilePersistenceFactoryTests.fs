namespace Aornota.Ubersweep.Tests.Server.Persistence

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Tests.Server.Common

open Expecto
open FsToolkit.ErrorHandling
open Microsoft.Extensions.Configuration
open Serilog
open System
open System.Collections.Generic
open System.IO

type TestPersistenceDir<'id, 'state, 'initEvent, 'event
    when 'id :> IId and 'state :> IState<'state, 'event> and 'initEvent :> IEvent and 'event :> IEvent>
    (
        partitionName: PartitionName option,
        snapshotFrequency: uint option,
        ?strictMode,
        ?skipCreatingDir,
        ?retainOnDispose
    ) =
    let strictMode = defaultArg strictMode false
    let skipCreatingDir = defaultArg skipCreatingDir false
    let retainOnDispose = defaultArg retainOnDispose false

    let relativeRoot = Path.Combine(@".\testDirs", $"{Guid.NewGuid()}")
    let entityName: EntityName = sanitize typeof<'state>

    let subPath =
        match partitionName with
        | Some partitionName -> Path.Combine(partitionName, entityName)
        | None -> entityName

    let dir = DirectoryInfo(Path.Combine(relativeRoot, subPath))

    do // create dir (if appropriate)
        if not (skipCreatingDir || dir.Exists) then
            dir.Create()

    let configDic = Dictionary<string, string>()

    let snapshotFrequency =
        snapshotFrequency
        |> Option.map (fun snapshotFrequency -> $"{int snapshotFrequency}")

    let strictMode = if strictMode then "true" else "false"

    do configDic.Add(FilePersistenceFactory.RelativeRootKey, relativeRoot)

    do // add snapshotFrequency to configDic (if appropriate)
        if snapshotFrequency.IsSome then
            configDic.Add(FilePersistenceFactory.SnapshotFrequencyKey, snapshotFrequency.Value)

    do configDic.Add(FilePersistenceFactory.StrictModeKey, $"{strictMode}")

    let config = ConfigurationBuilder().AddInMemoryCollection(configDic).Build()

    let factory = new FilePersistenceFactory(config, FixedClock.instance, Log.Logger)

    member _.Dir = dir
    member _.PathForError = $"...\{DirectoryInfo(relativeRoot).Name}\{subPath}"

    member _.Reader =
        (factory :> IPersistenceFactory).GetReader<'state, 'event> partitionName

    member _.Writer =
        (factory :> IPersistenceFactory).GetWriter<'state, 'event> partitionName

    member private _.TryWriteFileAsync(guid: Guid option, fileName: string, lines: string list) = asyncResult {
        try
            let path =
                match guid with
                | Some guid -> Path.Combine(dir.FullName, $"{guid}", fileName)
                | None -> Path.Combine(dir.FullName, fileName)

            let file = FileInfo path

            if File.Exists file.FullName then
                return! Error $"File {file.Name} already exists"

            return! File.WriteAllLinesAsync(file.FullName, lines)
        with exn ->
            return! Error $"Exception writing file {fileName} for {guid}: {exn.Message}"
    }

    member _.TryReadFileAsync(guid: Guid, fileName: string) = asyncResult {
        try
            let file = FileInfo(Path.Combine(dir.FullName, $"{guid}", fileName))

            if not (File.Exists file.FullName) then
                return! Error $"File {file.Name} does not exist"

            let! lines = File.ReadAllLinesAsync file.FullName

            return lines |> List.ofArray
        with exn ->
            return! Error $"Exception writing file {fileName} for {guid}: {exn.Message}"
    }

    member this.TryWriteFileAsync(guid: Guid, fileName: string, lines: string list) =
        this.TryWriteFileAsync(Some guid, fileName, lines)

    member this.TryWriteFileAsync(fileName: string, lines: string list) =
        this.TryWriteFileAsync(None, fileName, lines)

    interface IDisposable with
        member _.Dispose() : unit =
            (factory :> IDisposable).Dispose()

            if not retainOnDispose then
                let root = DirectoryInfo relativeRoot

                if root.Exists then
                    root.Delete true

[<RequireQualifiedAccess>]
module FilePersistenceFactoryTests =
    let private tryWriteFilesAsync
        (testDir: TestPersistenceDir<'id, 'state, 'initEvent, 'event>, guid: Guid, files: (string * string list) list)
        =
        asyncResult {
            let! result =
                files
                |> List.map (fun (fileName, lines) -> testDir.TryWriteFileAsync(guid, fileName, lines))
                |> Async.Parallel

            return!
                result
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.map (fun _ -> ())
                |> Result.mapError (fun errors -> $"One or more error when writing files: {errors}")
        }

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
                -- [ $"Directory does not exist when reading {guid} for {pathForError}" is not possible when tryReadAsync only called by tryReadAll? ]
                -- [ $"Directory is empty when reading {guid} for {pathForError}" is not possible when tryReadAsync only called by tryReadAll? ] *)
                testAsync "When files exist when reading all" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(None, None)

                    let fileName1, fileName2 = "test1.txt", "test2.txt"

                    let! result = asyncResult {
                        let! _ = testDir.TryWriteFileAsync(fileName1, [])
                        let! _ = testDir.TryWriteFileAsync(fileName2, [])
                        return! testDir.Reader.ReadAllAsync()
                    }

                    result
                    |> Check.isError
                        $"Files exist when reading all for {testDir.PathForError}: {[ fileName2; fileName1 ] |> List.sort}"
                }
                testAsync "When non-Guid directories exist when reading all" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(None, None)

                    let dir1, dir2 = "test1", "test2"

                    let! result = asyncResult {
                        Directory.CreateDirectory(Path.Combine(testDir.Dir.FullName, dir1)) |> ignore
                        Directory.CreateDirectory(Path.Combine(testDir.Dir.FullName, dir2)) |> ignore
                        return! testDir.Reader.ReadAllAsync()
                    }

                    result
                    |> Check.isError
                        $"Non-{nameof Guid} directories exist when reading all for {testDir.PathForError}: {[ dir1; dir2 ] |> List.sort}"
                }
            ]
            testList "CreateFromSnapshotAsync" [
            // TODO-TESTS: $"Directory for {guid} is not empty when creating from snapshot in {pathForError}"
            ]
            testList "WriteEventAsync" [
            (* TODO-TESTS:
                -- $"Directory for {guid} is enpty but {rvn} is not {Rvn.InitialRvn} when writing event for {guid} in {pathForError}"
                -- [ $"Strict mode error when writing event for {guid} for {pathForError}: {error}", e.g. if the last EventLine in the events file is not eventsFile.LastRvn? ]
                -- $"{rvn} is inconsistent with latest {nameof Rvn} ({eventsFile.LastRvn}) for events file {eventsFile.EventsFileName} when writing event for {guid} in {pathForError}"
                -- [ $"Strict mode error when writing event for {guid} for {pathForError}: {error}", e.g. if the only SnapshotLine in the latest snapshot file is not snapshotFile.Rvn? ]
                -- $"{rvn} is inconsistent with {nameof Rvn} ({snapshotFile.Rvn}) for latest snapshot file {snapshotFile.SnapshotFileName} when writing event for {guid} in {pathForError}" *)
            ]
        ]

    let private integration =
        testList "integration" [
        (* TODO-TESTS:
        *)
        ]

    let tests = testList $"{nameof FilePersistenceFactory}" [ happy; sad; integration ]
