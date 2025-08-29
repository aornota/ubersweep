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

    let relativeRoot = Path.Combine("testDirs", $"{Guid.NewGuid()}")
    let entityName: EntityName = sanitize typeof<'state>

    let subPath =
        match partitionName with
        | Some partitionName -> Path.Combine(partitionName, entityName)
        | None -> entityName

    let dir = DirectoryInfo(Path.Combine($@".\{relativeRoot}", subPath))

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

            if not (Directory.Exists file.Directory.FullName) then
                Directory.CreateDirectory file.Directory.FullName |> ignore
            else if File.Exists file.FullName then
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

    let private tryWriteEmptyFilesAsync
        (testDir: TestPersistenceDir<'id, 'state, 'initEvent, 'event>, guid: Guid, fileNames: string list)
        =
        asyncResult { return! tryWriteFilesAsync (testDir, guid, fileNames |> List.map (fun fileName -> fileName, [])) }

    let private partitionName = "2026"

    let private happy =
        testList "happy" [
            testList "ReadAllAsync" [
                // Note: Use partition for these.
                (* TODO-TESTS:
                *)
                testAsync "No Guid directories exist" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            None
                        )

                    let! result = asyncResult { return! testDir.Reader.ReadAllAsync() }

                    result |> Check.isOk []
                }
                testAsync "Single Guid directory with single events file" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            None
                        )

                    let guid = Guid.NewGuid()

                    let eventsFile =
                        addFileExtension Events "1-2",
                        [
                            """["EventLine",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["System","Test"],["Json","[\"Initialized\",-1]"]]"""
                            """["EventLine",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["System","Test"],["Json","\"Incremented\""]]"""
                        ]

                    let! result = asyncResult {
                        let! _ = tryWriteFilesAsync (testDir, guid, [ eventsFile ])
                        return! testDir.Reader.ReadAllAsync()
                    }

                    result
                    |> Check.isOk [
                        guid,
                        NonEmptyList<Entry'>
                            .Create(
                                EventJson(Rvn 1u, fixedUtcNow, sourceSystemTest, (Initialized -1 :> IEvent).EventJson),
                                [
                                    EventJson(Rvn 2u, fixedUtcNow, sourceSystemTest, (Incremented :> IEvent).EventJson)
                                ]
                            )
                    ]
                }
            ]
            testList "CreateFromSnapshotAsync" [
            // Note: Use partition for these.
            (* TODO-TESTS:
            *)
            ]
            testList "WriteEventAsync" [
            // Note: Do not use partition for these.
            (* TODO-TESTS:
            *)
            ]
        ]

    let private sad =
        testList "sad" [
            testList "ReadAllAsync" [
                // Note: Cannot cause "Directory does not exist when reading {guid} for {pathForError}" error in tryReadAsync as this is only called by tryReadAll for Guid directories that do exist.
                testAsync "Files exist" {
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
                testAsync "Non-Guid directories exist" {
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
                testAsync "Empty Guid directories exist" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(None, None)

                    let guid1, guid2 = Guid.NewGuid(), Guid.NewGuid()

                    let! result = asyncResult {
                        Directory.CreateDirectory(Path.Combine(testDir.Dir.FullName, $"{guid1}"))
                        |> ignore

                        Directory.CreateDirectory(Path.Combine(testDir.Dir.FullName, $"{guid2}"))
                        |> ignore

                        return! testDir.Reader.ReadAllAsync()
                    }

                    let errors =
                        [ guid1; guid2 ]
                        |> List.sort
                        |> List.map (fun guid -> $"Directory is empty when reading {guid} for {testDir.PathForError}")

                    result
                    |> Check.isError $"One or more error when reading all for {testDir.PathForError}: {errors}"
                }
            ]
            testList "CreateFromSnapshotAsync" [
                testAsync "Guid directory is not empty" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(None, None)

                    let guid = Guid.NewGuid()

                    let! result = asyncResult {
                        let! _ =
                            tryWriteEmptyFilesAsync (
                                testDir,
                                guid,
                                [ addFileExtension Events "1-36"; addFileExtension Snapshot "36" ]
                            )

                        return! testDir.Writer.CreateFromSnapshotAsync(guid, Rvn.InitialRvn, Json """{"Count":666}""")
                    }

                    result
                    |> Check.isError
                        $"Directory for {guid} contains events files and/or snapshot files when creating from snapshot in {testDir.PathForError}"
                }
            ]
            testList "WriteEventAsync" [
                testAsync "Guid directory does not exist but revision is not initial revision" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(None, None)

                    let guid = Guid.NewGuid()

                    let! result = asyncResult {
                        return! testDir.Writer.WriteEventAsync(guid, Rvn 5u, sourceUser1, Incremented, None)
                    }

                    result
                    |> Check.isError
                        $"Directory for {guid} does not exist but {Rvn 5u} is not {Rvn.InitialRvn} when writing event for {guid} in {testDir.PathForError}"
                }
                testAsync "Guid directory is empty but revision is not initial revision" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(None, None)

                    let guid = Guid.NewGuid()

                    let! result = asyncResult {
                        Directory.CreateDirectory(Path.Combine(testDir.Dir.FullName, $"{guid}"))
                        |> ignore

                        return! testDir.Writer.WriteEventAsync(guid, Rvn 5u, sourceUser1, Incremented, None)
                    }

                    result
                    |> Check.isError
                        $"Directory for {guid} is empty but {Rvn 5u} is not {Rvn.InitialRvn} when writing event for {guid} in {testDir.PathForError}"
                }
                testAsync "Revision is not contiguous with latest revision for latest events file" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(None, None)

                    let guid = Guid.NewGuid()

                    let eventsFileName = addFileExtension Events "1-6"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ eventsFileName ])
                        return! testDir.Writer.WriteEventAsync(guid, Rvn 8u, sourceUser1, Incremented, None)
                    }

                    result
                    |> Check.isError
                        $"{Rvn 8u} is not contiguous with latest {nameof Rvn} ({Rvn 6u}) for latest events file {eventsFileName} when writing event for {guid} in {testDir.PathForError}"
                }
                testAsync "Revision is not contiguous with revision for latest snapshot file" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(None, None)

                    let guid = Guid.NewGuid()

                    let snapshotFlleName = addFileExtension Snapshot "1"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ snapshotFlleName ])
                        return! testDir.Writer.WriteEventAsync(guid, Rvn 1u, sourceUser1, Initialized 0, None)
                    }

                    result
                    |> Check.isError
                        $"{Rvn 1u} is not contiguous with {nameof Rvn} ({Rvn 1u}) for latest snapshot file {snapshotFlleName} when writing event for {guid} in {testDir.PathForError}"
                }
                testAsync "Strict mode error for single empty events file (with initial first revision)" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            None,
                            None,
                            strictMode = true
                        )

                    let guid = Guid.NewGuid()

                    let eventsFileName = addFileExtension Events "1-4"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ eventsFileName ])
                        return! testDir.Writer.WriteEventAsync(guid, Rvn 5u, sourceUser1, Incremented, None)
                    }

                    let error = $"Events file {eventsFileName} is empty"

                    result
                    |> Check.isError
                        $"Strict mode error when writing event for {guid} for {testDir.PathForError}: {error}"
                }
                testAsync "Strict mode error for single empty snapshot file" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            None,
                            None,
                            strictMode = true
                        )

                    let guid = Guid.NewGuid()

                    let snapshotFlleName = addFileExtension Snapshot "48"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ snapshotFlleName ])
                        return! testDir.Writer.WriteEventAsync(guid, Rvn 49u, sourceUser1, Incremented, None)
                    }

                    let error = $"Snapshot file {snapshotFlleName} is empty"

                    result
                    |> Check.isError
                        $"Strict mode error when writing event for {guid} for {testDir.PathForError}: {error}"
                }
            ]
        ]

    let private integration =
        testList "integration" [
        (* TODO-TESTS:
        *)
        ]

    let tests = testList $"{nameof FilePersistenceFactory}" [ happy; sad; integration ]
