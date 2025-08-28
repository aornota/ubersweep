namespace Aornota.Ubersweep.Tests.Server.Persistence

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Tests.Server.Common

open Expecto
open FsToolkit.ErrorHandling
open System
open System.IO

type private FileType =
    | Events
    | Snapshot

type private TestPersistenceDir(createDirForGuid: Guid option, ?retainOnDispose) =
    let retainOnDispose = defaultArg retainOnDispose false

    let path = Path.Combine(@".\testDirs", Guid.NewGuid().ToString())

    let dir = DirectoryInfo path

    do dir.Create()

    do // create directory for guid (if appropriate)
        match createDirForGuid with
        | Some guid ->
            let pathForGuid = Path.Combine(dir.FullName, guid.ToString())
            (DirectoryInfo pathForGuid).Create()
        | None -> ()

    member _.Dir = dir
    member _.PathForError(guid: Guid) = $@"...\{dir.Name}\{guid}"

    member _.TryWriteEmptyFileAsync(guid: Guid, fileName: string) = asyncResult {
        try
            let file = FileInfo(Path.Combine(path, guid.ToString(), fileName))

            if File.Exists file.FullName then
                return! Error $"File {file.Name} already exists"

            return! File.WriteAllLinesAsync(file.FullName, [])

        with exn ->
            return! Error $"Exception writing file for {guid}: {exn.Message}"
    }

    member _.TryWriteEventsFileAsync(guid: Guid, firstRvn: uint, lastRvn: uint, lines: string list) = asyncResult {
        try
            let! eventsFileName = FilePersistence.getEventsFileName (Rvn firstRvn, Rvn lastRvn)

            let file = FileInfo(Path.Combine(path, guid.ToString(), eventsFileName))

            if File.Exists file.FullName then
                return! Error $"File {file.Name} already exists"

            return! File.WriteAllLinesAsync(file.FullName, lines)

        with exn ->
            return! Error $"Exception writing events file ({firstRvn}-{lastRvn}) for {guid}: {exn.Message}"
    }

    member _.TryWriteSnapshotFileAsync(guid: Guid, rvn: uint, lines: string list) = asyncResult {
        try
            let! snapshotFileName = FilePersistence.getSnapshotFileName (Rvn rvn)

            let file = FileInfo(Path.Combine(path, guid.ToString(), snapshotFileName))

            if File.Exists file.FullName then
                return! Error $"File {file.Name} already exists"

            return! File.WriteAllLinesAsync(file.FullName, lines)

        with exn ->
            return! Error $"Exception writing snapshot file ({rvn}) for {guid}: {exn.Message}"
    }

    interface IDisposable with
        member _.Dispose() : unit =
            if not retainOnDispose && dir.Exists then
                dir.Delete true

[<RequireQualifiedAccess>]
module FilePersistenceTests =
    let private tryWriteEmptyFilesAsync (testDir: TestPersistenceDir, guid: Guid, fileNames: string list) = asyncResult {
        let! result =
            fileNames
            |> List.map (fun fileName -> testDir.TryWriteEmptyFileAsync(guid, fileName))
            |> Async.Parallel

        return!
            result
            |> List.ofArray
            |> List.sequenceResultA
            |> Result.map (fun _ -> ())
            |> Result.mapError (fun errors -> $"One or more error when writing empty files: {errors}")
    }

    let private tryWriteEventsFilesAsync
        (testDir: TestPersistenceDir, guid: Guid, eventsFileDetails: (uint * uint * string list) list)
        =
        asyncResult {
            let! result =
                eventsFileDetails
                |> List.map (fun (firstRvn, lastRvn, lines) ->
                    testDir.TryWriteEventsFileAsync(guid, firstRvn, lastRvn, lines))
                |> Async.Parallel

            return!
                result
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.map (fun _ -> ())
                |> Result.mapError (fun errors -> $"One or more error when writing events files: {errors}")
        }

    let private tryWriteSnapshotFilesAsync
        (testDir: TestPersistenceDir, guid: Guid, snapshotFileDetails: (uint * string list) list)
        =
        asyncResult {
            let! result =
                snapshotFileDetails
                |> List.map (fun (rvn, lines) -> testDir.TryWriteSnapshotFileAsync(guid, rvn, lines))
                |> Async.Parallel

            return!
                result
                |> List.ofArray
                |> List.sequenceResultA
                |> Result.map (fun _ -> ())
                |> Result.mapError (fun errors -> $"One or more error when writing snapshot files: {errors}")
        }

    let private addFileExtension fileType (fileName: string) =
        match fileType with
        | Events -> $"{fileName}.{FilePersistence.eventsFileExtension}"
        | Snapshot -> $"{fileName}.{FilePersistence.snapshotFileExtension}"

    let private happy =
        testList "happy" [
            testList "getEventsFileName" [
                test $"When last {nameof Rvn} equals first {nameof Rvn}" {
                    FilePersistence.getEventsFileName (Rvn 1u, Rvn 1u)
                    |> Check.isOk $"1-1.{FilePersistence.eventsFileExtension}"
                }
                test $"When last {nameof Rvn} is greater than first {nameof Rvn}" {
                    FilePersistence.getEventsFileName (Rvn 66u, Rvn 69u)
                    |> Check.isOk $"66-69.{FilePersistence.eventsFileExtension}"
                }
            ]
            testList "getSnapshotFileName" [
                test $"When {nameof Rvn} is valid" {
                    FilePersistence.getSnapshotFileName (Rvn 52u)
                    |> Check.isOk $"52.{FilePersistence.snapshotFileExtension}"
                }
            ]
            testList "getDirStatusAsync (not strict mode)" [
                // Note: Can use empty files since not strict mode.
                testAsync "When directory for Guid does not exist" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(None)

                    let! result = FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)

                    result |> Check.isOk DoesNotExist
                }
                testAsync "When directory for Guid is empty" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let! result = FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)

                    result |> Check.isOk Empty
                }
                testAsync "When single events file with initial first revision" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileName = addFileExtension Events "1-9"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ eventsFileName ])
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isOk (
                        EventsOnly {
                            EventsFileName = eventsFileName
                            FirstRvn = Rvn 1u
                            LastRvn = Rvn 9u
                        }
                    )
                }
                testAsync "When single snapshot file with initial revision" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let snapshotFileName = addFileExtension Snapshot "1"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ snapshotFileName ])
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isOk (
                        SnapshotOnly {
                            SnapshotFileName = snapshotFileName
                            Rvn = Rvn 1u
                        }
                    )
                }
                testAsync "When single snapshot file with non-initial revision" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let snapshotFileName = addFileExtension Snapshot "666"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ snapshotFileName ])
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isOk (
                        SnapshotOnly {
                            SnapshotFileName = snapshotFileName
                            Rvn = Rvn 666u
                        }
                    )
                }
                testAsync "When single snapshot file with initial revision and subsequent contiguous events file" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let snapshotFileName = addFileExtension Snapshot "1"
                    let eventsFileName = addFileExtension Events "2-2"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ snapshotFileName; eventsFileName ])
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isOk (
                        SnapshotAndEvents(
                            {
                                SnapshotFileName = snapshotFileName
                                Rvn = Rvn 1u
                            },
                            {
                                EventsFileName = eventsFileName
                                FirstRvn = Rvn 2u
                                LastRvn = Rvn 2u
                            }
                        )
                    )
                }
                testAsync "When single snapshot file with non-initial revision and subsequent contiguous events file" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileName1 = addFileExtension Events "1-36"
                    let snapshotFileName = addFileExtension Snapshot "36"
                    let eventsFileName2 = addFileExtension Events "37-52"

                    let! result = asyncResult {
                        let! _ =
                            tryWriteEmptyFilesAsync (
                                testDir,
                                guid,
                                [ eventsFileName1; snapshotFileName; eventsFileName2 ]
                            )

                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isOk (
                        SnapshotAndEvents(
                            {
                                SnapshotFileName = snapshotFileName
                                Rvn = Rvn 36u
                            },
                            {
                                EventsFileName = eventsFileName2
                                FirstRvn = Rvn 37u
                                LastRvn = Rvn 52u
                            }
                        )
                    )
                }
                testAsync "When multiple snapshot files and no subsequent events file for last snapshot file" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileName1 = addFileExtension Events "1-36"
                    let snapshotFileName1 = addFileExtension Snapshot "36"
                    let eventsFileName2 = addFileExtension Events "37-52"
                    let snapshotFileName2 = addFileExtension Snapshot "52"

                    let! result = asyncResult {
                        let! _ =
                            tryWriteEmptyFilesAsync (
                                testDir,
                                guid,
                                [ eventsFileName1; snapshotFileName1; eventsFileName2; snapshotFileName2 ]
                            )

                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isOk (
                        SnapshotOnly {
                            SnapshotFileName = snapshotFileName2
                            Rvn = Rvn 52u
                        }
                    )
                }
                testAsync "When multiple snapshot files and subsequent contiguous events file for last snapshot file" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileName1 = addFileExtension Events "1-36"
                    let snapshotFileName1 = addFileExtension Snapshot "36"
                    let eventsFileName2 = addFileExtension Events "37-52"
                    let snapshotFileName2 = addFileExtension Snapshot "52"
                    let eventsFileName3 = addFileExtension Events "53-53"

                    let! result = asyncResult {
                        let! _ =
                            tryWriteEmptyFilesAsync (
                                testDir,
                                guid,
                                [
                                    eventsFileName1
                                    snapshotFileName1
                                    eventsFileName2
                                    snapshotFileName2
                                    eventsFileName3
                                ]
                            )

                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isOk (
                        SnapshotAndEvents(
                            {
                                SnapshotFileName = snapshotFileName2
                                Rvn = Rvn 52u
                            },
                            {
                                EventsFileName = eventsFileName3
                                FirstRvn = Rvn 53u
                                LastRvn = Rvn 53u
                            }
                        )
                    )
                }
            ]
        (* TODO-TESTS:
                -- tryDecodeEventsFileAsync...
                -- tryDecodeSnapshotFileAsync...
                -- getDirStatusAsync (strict mode)?... *)
        ]

    let private sad =
        testList "sad" [
            testList "getEventsFileName" [
                test $"When first {nameof Rvn} is {0u}" {
                    FilePersistence.getEventsFileName (Rvn 0u, Rvn 1u)
                    |> Check.isError $"First {nameof Rvn} for name of events file must not be {Rvn 0u}"
                }
                test $"When last {nameof Rvn} is {0u}" {
                    FilePersistence.getEventsFileName (Rvn 1u, Rvn 0u)
                    |> Check.isError $"Last {nameof Rvn} for name of events file must not be {Rvn 0u}"
                }
                test $"When first {nameof Rvn} is greater than last {nameof Rvn}" {
                    let first, last = 69u, 66u

                    FilePersistence.getEventsFileName (Rvn first, Rvn last)
                    |> Check.isError
                        $"First {nameof Rvn} ({Rvn first}) must not be greater than last {nameof Rvn} ({Rvn last}) for name of events file"
                }
            ]
            testList "getSnapshotFileName" [
                test $"When {nameof Rvn} is {0u}" {
                    FilePersistence.getSnapshotFileName (Rvn 0u)
                    |> Check.isError $"{nameof Rvn} for name of snapshot file must not be {Rvn 0u}"
                }
            ]
            testList "getDirStatusAsync (not strict mode)" [
                // Note: Can use empty files since not strict mode.
                testAsync "When files with invalid extensions" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let fileNamesWithInvalidExtensions = [ "1-9.evonts"; "9.snipshot" ]

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, fileNamesWithInvalidExtensions)
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isError
                        $"One or more file with an invalid extension in {testDir.PathForError guid}: {fileNamesWithInvalidExtensions |> List.sort}"
                }
                testAsync "When events files with invalid names" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let invalidEventsFileNames =
                        [ "0-1"; "1.5-1"; "a-1"; "1-0"; "1-1.5"; "1-a"; "2-1"; "1-"; "-1"; "1"; "a" ]
                        |> List.map (addFileExtension Events)

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, invalidEventsFileNames)
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    let expectedErrors =
                        invalidEventsFileNames
                        |> List.sort
                        |> List.map (fun fileName -> $"Invalid name for events file {fileName}")

                    result
                    |> Check.isError
                        $"One or more events file with an invalid name in {testDir.PathForError guid}: {expectedErrors}"
                }
                testAsync "When snapshot files with invalid names" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let invalidSnapshotFileNames =
                        [ "0"; "1-"; "-1"; "1.5"; "a" ] |> List.map (addFileExtension Snapshot)

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, invalidSnapshotFileNames)
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    let expectedErrors =
                        invalidSnapshotFileNames
                        |> List.sort
                        |> List.map (fun fileName -> $"Invalid name for snapshot file {fileName}")

                    result
                    |> Check.isError
                        $"One or more snapshot file with an invalid name in {testDir.PathForError guid}: {expectedErrors}"
                }
                testAsync "When single events file with non-initial first revision" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileName = addFileExtension Events "2-9"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ eventsFileName ])
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isError
                        $"First {nameof Rvn} ({Rvn 2u}) is not {Rvn.InitialRvn} for only events file {eventsFileName} in {testDir.PathForError guid}"
                }
                testAsync "When multiple events files and no snapshot files" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileNames = [ "1-5"; "6-6" ] |> List.map (addFileExtension Events)

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, eventsFileNames)
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isError
                        $"There are multiple events files and no snapshot files in {testDir.PathForError guid}: {eventsFileNames |> List.sort}"
                }
                testAsync "When multiple snapshot files and no events files" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let snapshotFileNames = [ "1"; "2" ] |> List.map (addFileExtension Snapshot)

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, snapshotFileNames)
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isError
                        $"There are multiple snapshot files and no events files in {testDir.PathForError guid}: {snapshotFileNames |> List.sort}"
                }
                testAsync "When first events file with non-initial first revision (and no previous snapshot file)" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileName = addFileExtension Events "2-9"
                    let snapshotFileName = addFileExtension Snapshot "10"

                    let! result = asyncResult {
                        let! _ = tryWriteEmptyFilesAsync (testDir, guid, [ eventsFileName; snapshotFileName ])
                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isError
                        $"First {nameof Rvn} ({Rvn 2u}) is not {Rvn.InitialRvn} for first events file {eventsFileName} (and no previous snapshot file) in {testDir.PathForError guid}"
                }
                testAsync "When snapshot file follows snapshot file" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileName = addFileExtension Events "1-5"
                    let snapshotFileName1 = addFileExtension Snapshot "5"
                    let snapshotFileName2 = addFileExtension Snapshot "7"

                    let! result = asyncResult {
                        let! _ =
                            tryWriteEmptyFilesAsync (
                                testDir,
                                guid,
                                [ eventsFileName; snapshotFileName1; snapshotFileName2 ]
                            )

                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isError
                        $"Snaphot file {snapshotFileName2} follows snapshot file {snapshotFileName1} in {testDir.PathForError guid}"
                }
                testAsync "When events file follows events file" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let snapshotFileName = addFileExtension Snapshot "3"
                    let eventsFileName1 = addFileExtension Events "4-8"
                    let eventsFileName2 = addFileExtension Events "9-11"

                    let! result = asyncResult {
                        let! _ =
                            tryWriteEmptyFilesAsync (
                                testDir,
                                guid,
                                [ snapshotFileName; eventsFileName1; eventsFileName2 ]
                            )

                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isError
                        $"Events file {eventsFileName2} follows events file {eventsFileName1} in {testDir.PathForError guid}"
                }
                testAsync "When revision for snapshot file not the same as last revision for previous events file" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileName1 = addFileExtension Events "1-100"
                    let snapshotFileName1 = addFileExtension Snapshot "100"
                    let eventsFileName2 = addFileExtension Events "101-109"
                    let snapshotFileName2 = addFileExtension Snapshot "111"

                    let! result = asyncResult {
                        let! _ =
                            tryWriteEmptyFilesAsync (
                                testDir,
                                guid,
                                [ eventsFileName1; snapshotFileName1; eventsFileName2; snapshotFileName2 ]
                            )

                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isError
                        $"{nameof Rvn} ({Rvn 111u}) for snapshot file {snapshotFileName2} is not the same as last {nameof Rvn} ({Rvn 109u}) for previous events file {eventsFileName2} in {testDir.PathForError guid}"
                }
                testAsync "When first revision for events file not contiguous with revision for previous snapshot file" {
                    let guid = Guid.NewGuid()
                    use testDir = new TestPersistenceDir(Some guid)

                    let eventsFileName1 = addFileExtension Events "1-100"
                    let snapshotFileName = addFileExtension Snapshot "100"
                    let eventsFileName2 = addFileExtension Events "100-109"

                    let! result = asyncResult {
                        let! _ =
                            tryWriteEmptyFilesAsync (
                                testDir,
                                guid,
                                [ eventsFileName1; snapshotFileName; eventsFileName2 ]
                            )

                        return! FilePersistence.getDirStatusAsync (testDir.Dir, guid, false)
                    }

                    result
                    |> Check.isError
                        $"First {nameof Rvn} ({Rvn 100u}) for events file {eventsFileName2} is not contiguous with {nameof Rvn} ({Rvn 100u}) for previous snapshot file {snapshotFileName} in {testDir.PathForError guid}"
                }
            ]
        (* TODO-TESTS:
                -- tryDecodeEventsFileAsync...
                -- tryDecodeSnapshotFileAsync...
                -- getDirStatusAsync (strict mode)?... *)
        ]

    let tests = testList $"{nameof FilePersistence}" [ happy; sad ]
