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
                testAsync "Single Guid directory with single snapshot file" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            None
                        )

                    let guid = Guid.NewGuid()

                    let snapshotFile =
                        addFileExtension Snapshot "1", [ """["SnapshotLine",["Rvn",1],["Json","{\"Count\":1}"]]""" ]

                    let! result = asyncResult {
                        let! _ = tryWriteFilesAsync (testDir, guid, [ snapshotFile ])
                        return! testDir.Reader.ReadAllAsync()
                    }

                    result
                    |> Check.isOk [
                        guid,
                        NonEmptyList<Entry'>
                            .Create(
                                SnapshotJson(Rvn 1u, ({ Count = 1 } :> IState<Counter, CounterEvent>).SnapshotJson),
                                []
                            )
                    ]
                }
                testAsync "Multiple Guid directories" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            None
                        )

                    let guid1, guid2 = Guid.NewGuid(), Guid.NewGuid()

                    let snapshotFile_1 =
                        addFileExtension Snapshot "5", [ """["SnapshotLine",["Rvn",5],["Json","{\"Count\":1}"]]""" ]

                    let eventsFile_1 =
                        addFileExtension Events "6-7",
                        [
                            """["EventLine",["Rvn",6],"2025-08-07T15:11:33.0000000Z",["System","Test"],["Json","\"Incremented\""]]"""
                            """["EventLine",["Rvn",7],"2025-08-07T15:11:33.0000000Z",["System","Test"],["Json","\"Incremented\""]]"""
                        ]

                    let eventsFile1_2 =
                        addFileExtension Events "1-2",
                        [
                            """["EventLine",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["System","Test"],["Json","[\"Initialized\",-1]"]]"""
                            """["EventLine",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["System","Test"],["Json","\"Incremented\""]]"""
                        ]

                    let snapshotFile_2 =
                        addFileExtension Snapshot "2", [ """["SnapshotLine",["Rvn",2],["Json","{\"Count\":0}"]]""" ]

                    let eventsFile2_2 =
                        addFileExtension Events "3-4",
                        [
                            """["EventLine",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["System","Test"],["Json","\"Decremented\""]]"""
                            """["EventLine",["Rvn",4],"2025-08-07T15:11:33.0000000Z",["System","Test"],["Json","\"Decremented\""]]"""
                        ]

                    let! result = asyncResult {
                        let! _ = tryWriteFilesAsync (testDir, guid1, [ snapshotFile_1; eventsFile_1 ])
                        let! _ = tryWriteFilesAsync (testDir, guid2, [ eventsFile1_2; snapshotFile_2; eventsFile2_2 ])
                        return! testDir.Reader.ReadAllAsync()
                    }

                    let expected = [
                        guid1,
                        NonEmptyList<Entry'>
                            .Create(
                                SnapshotJson(Rvn 5u, ({ Count = 1 } :> IState<Counter, CounterEvent>).SnapshotJson),
                                [
                                    EventJson(Rvn 6u, fixedUtcNow, sourceSystemTest, (Incremented :> IEvent).EventJson)
                                    EventJson(Rvn 7u, fixedUtcNow, sourceSystemTest, (Incremented :> IEvent).EventJson)
                                ]
                            )
                        guid2,
                        NonEmptyList<Entry'>
                            .Create(
                                SnapshotJson(Rvn 2u, ({ Count = 0 } :> IState<Counter, CounterEvent>).SnapshotJson),
                                [
                                    EventJson(Rvn 3u, fixedUtcNow, sourceSystemTest, (Decremented :> IEvent).EventJson)
                                    EventJson(Rvn 4u, fixedUtcNow, sourceSystemTest, (Decremented :> IEvent).EventJson)
                                ]
                            )
                    ]

                    result |> Check.isOk (expected |> List.sortBy fst)
                }
            ]
            testList "CreateFromSnapshotAsync" [
                // Note: Use partition for these.
                testAsync "Initial revision" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            None
                        )

                    let guid = Guid.NewGuid()

                    let! result = asyncResult {
                        let! _ = testDir.Writer.CreateFromSnapshotAsync(guid, Rvn.InitialRvn, Json """{"Count":666}""")
                        return! testDir.TryReadFileAsync(guid, addFileExtension Snapshot "1")
                    }

                    result
                    |> Check.isOk [ """["SnapshotLine",["Rvn",1],["Json","{\"Count\":666}"]]""" ]
                }
                testAsync "Non-initial revision" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            None
                        )

                    let guid = Guid.NewGuid()

                    let! result = asyncResult {
                        let! _ = testDir.Writer.CreateFromSnapshotAsync(guid, Rvn 2048u, Json """{"Count":8192}""")
                        return! testDir.TryReadFileAsync(guid, addFileExtension Snapshot "2048")
                    }

                    result
                    |> Check.isOk [ """["SnapshotLine",["Rvn",2048],["Json","{\"Count\":8192}"]]""" ]
                }
            ]
            testList "WriteEventAsync" [
                // Note: Do not use partition for these.
                testAsync "Single (initial revision) event with no existing events or snapshot files" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            None
                        )

                    let guid = Guid.NewGuid()

                    let! result = asyncResult {
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 1u, sourceUser1, Incremented, None)
                        return! testDir.TryReadFileAsync(guid, addFileExtension Events "1-1")
                    }

                    result
                    |> Check.isOk [
                        $"""["EventLine",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","\"Incremented\""]]"""
                    ]
                }
                testAsync "Single (non-initial revision) event with latest events file" {
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
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 3u, sourceUser2, MultipliedBy 2, None)
                        return! testDir.TryReadFileAsync(guid, addFileExtension Events "1-3")
                    }

                    let expected =
                        snd eventsFile
                        @ [
                            $"""["EventLine",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","[\"MultipliedBy\",2]"]]"""
                        ]

                    result |> Check.isOk expected
                }
                testAsync "Single (non-initial revision) event with latest snapshot file" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            None
                        )

                    let guid = Guid.NewGuid()

                    let snapshotFile =
                        addFileExtension Snapshot "5", [ """["SnapshotLine",["Rvn",5],["Json","{\"Count\":7}"]]""" ]

                    let! result = asyncResult {
                        let! _ = tryWriteFilesAsync (testDir, guid, [ snapshotFile ])
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 6u, sourceUser1, MultipliedBy 2, None)
                        let! snapshotFileLines = testDir.TryReadFileAsync(guid, fst snapshotFile)
                        let! eventsFileLines = testDir.TryReadFileAsync(guid, addFileExtension Events "6-6")
                        return! Ok(snapshotFileLines, eventsFileLines)
                    }

                    result
                    |> Check.isOk (
                        snd snapshotFile,
                        [
                            $"""["EventLine",["Rvn",6],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","[\"MultipliedBy\",2]"]]"""
                        ]
                    )
                }
                testAsync "Multiple events with snapshotting not enabled" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            None
                        )

                    let guid = Guid.NewGuid()

                    let! result = asyncResult {
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 1u, sourceUser1, Initialized 0, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 2u, sourceUser2, Incremented, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 3u, sourceUser2, Incremented, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 4u, sourceUser2, MultipliedBy 4, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 5u, sourceUser1, Decremented, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 6u, sourceUser1, DividedBy 2, None)
                        return! testDir.TryReadFileAsync(guid, addFileExtension Events "1-6")
                    }

                    result
                    |> Check.isOk [
                        $"""["EventLine",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","[\"Initialized\",0]"]]"""
                        $"""["EventLine",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","\"Incremented\""]]"""
                        $"""["EventLine",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","\"Incremented\""]]"""
                        $"""["EventLine",["Rvn",4],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","[\"MultipliedBy\",4]"]]"""
                        $"""["EventLine",["Rvn",5],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","\"Decremented\""]]"""
                        $"""["EventLine",["Rvn",6],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","[\"DividedBy\",2]"]]"""
                    ]
                }
                testAsync "Multiple events with snapshotting enabled and snapshots provided" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            Some 5u
                        )

                    let guid = Guid.NewGuid()

                    let! result = asyncResult {
                        let! _ =
                            testDir.Writer.WriteEventAsync(
                                guid,
                                Rvn 1u,
                                sourceUser1,
                                Initialized 0,
                                Some(fun _ -> ({ Count = 0 } :> IState<Counter, CounterEvent>).SnapshotJson)
                            )

                        let! _ =
                            testDir.Writer.WriteEventAsync(
                                guid,
                                Rvn 2u,
                                sourceUser2,
                                Incremented,
                                Some(fun _ -> ({ Count = 1 } :> IState<Counter, CounterEvent>).SnapshotJson)
                            )

                        let! _ =
                            testDir.Writer.WriteEventAsync(
                                guid,
                                Rvn 3u,
                                sourceUser2,
                                Incremented,
                                Some(fun _ -> ({ Count = 2 } :> IState<Counter, CounterEvent>).SnapshotJson)
                            )

                        let! _ =
                            testDir.Writer.WriteEventAsync(
                                guid,
                                Rvn 4u,
                                sourceUser2,
                                MultipliedBy 4,
                                Some(fun _ -> ({ Count = 8 } :> IState<Counter, CounterEvent>).SnapshotJson)
                            )

                        let! _ =
                            testDir.Writer.WriteEventAsync(
                                guid,
                                Rvn 5u,
                                sourceUser1,
                                Decremented,
                                Some(fun _ -> ({ Count = 7 } :> IState<Counter, CounterEvent>).SnapshotJson)
                            )

                        let! _ =
                            testDir.Writer.WriteEventAsync(
                                guid,
                                Rvn 6u,
                                sourceUser1,
                                DividedBy 2,
                                Some(fun _ -> ({ Count = 3 } :> IState<Counter, CounterEvent>).SnapshotJson)
                            )

                        let! eventsLines1 = testDir.TryReadFileAsync(guid, addFileExtension Events "1-5")
                        let! snapshotLines = testDir.TryReadFileAsync(guid, addFileExtension Snapshot "5")
                        let! eventsLines2 = testDir.TryReadFileAsync(guid, addFileExtension Events "6-6")

                        return! Ok(eventsLines1, snapshotLines, eventsLines2)
                    }

                    result
                    |> Check.isOk (
                        [
                            $"""["EventLine",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","[\"Initialized\",0]"]]"""
                            $"""["EventLine",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","\"Incremented\""]]"""
                            $"""["EventLine",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","\"Incremented\""]]"""
                            $"""["EventLine",["Rvn",4],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","[\"MultipliedBy\",4]"]]"""
                            $"""["EventLine",["Rvn",5],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","\"Decremented\""]]"""
                        ],
                        [ """["SnapshotLine",["Rvn",5],["Json","{\"Count\":7}"]]""" ],
                        [
                            $"""["EventLine",["Rvn",6],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","[\"DividedBy\",2]"]]"""
                        ]
                    )
                }
                testAsync "Multiple events with snapshotting enabled but snapshots not provided" {
                    use testDir =
                        new TestPersistenceDir<CounterId, Counter, CounterInitEvent, CounterEvent>(
                            Some partitionName,
                            Some 5u
                        )

                    let guid = Guid.NewGuid()

                    let! result = asyncResult {
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 1u, sourceUser1, Initialized 0, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 2u, sourceUser2, Incremented, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 3u, sourceUser2, Incremented, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 4u, sourceUser2, MultipliedBy 4, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 5u, sourceUser1, Decremented, None)
                        let! _ = testDir.Writer.WriteEventAsync(guid, Rvn 6u, sourceUser1, DividedBy 2, None)
                        return! testDir.TryReadFileAsync(guid, addFileExtension Events "1-6")
                    }

                    result
                    |> Check.isOk [
                        $"""["EventLine",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","[\"Initialized\",0]"]]"""
                        $"""["EventLine",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","\"Incremented\""]]"""
                        $"""["EventLine",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","\"Incremented\""]]"""
                        $"""["EventLine",["Rvn",4],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId2 :> IId).Guid}"]],["Json","[\"MultipliedBy\",4]"]]"""
                        $"""["EventLine",["Rvn",5],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","\"Decremented\""]]"""
                        $"""["EventLine",["Rvn",6],"2025-08-07T15:11:33.0000000Z",["User",["UserId","{(userId1 :> IId).Guid}"]],["Json","[\"DividedBy\",2]"]]"""
                    ]
                }
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
                testAsync "Guid directory does not exist but revision is non-initial revision" {
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
                testAsync "Guid directory is empty but revision is non-initial revision" {
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
