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

    let path = Path.Combine($@".\testDirs", Guid.NewGuid().ToString())

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
            ]
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
                        $"One or more file with an invalid extension in {testDir.PathForError guid}: {fileNamesWithInvalidExtensions}"
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
            ]
        ]

    let tests = testList $"{nameof FilePersistence}" [ happy; sad ]
