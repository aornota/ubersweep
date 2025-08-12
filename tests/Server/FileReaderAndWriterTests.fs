namespace Aornota.Ubersweep.Tests.Server

open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain.Entities
open Aornota.Ubersweep.Tests.Server.Common

open Expecto
open FsToolkit.ErrorHandling
open System

[<RequireQualifiedAccess>]
module FileReaderAndWriterTests =
    let private initializeAndApply
        (guid, initEventAndAuditUserId, eventsAndAuditUserIds, testDir: TestPersistenceDirectory<Counter>)
        =
        asyncResult {
            let rec apply eventsAndAuditUserIds counter = asyncResult {
                match eventsAndAuditUserIds with
                | (event, auditUserId) :: t ->
                    let! counter, event = counter |> Counter.apply event
                    let! _ = testDir.WriteAsync(counter, event, auditUserId)
                    return! apply t counter
                | [] -> return counter
            }

            let counter, event =
                Counter.initializeFromCommand (guid, fst initEventAndAuditUserId)

            let! _ = testDir.WriteAsync(counter, event, snd initEventAndAuditUserId)

            return! apply eventsAndAuditUserIds counter
        }

    let private happy =
        testList "happy" [
            testAsync "Read (initial event entry) with no partition" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    return! testDir.ReadAsync guid
                }

                match result with
                | Ok nonEmptyList ->
                    let expectedNonEmptyList =
                        NonEmptyList<Entry>
                            .Create(
                                EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                []
                            )

                    Expect.equal nonEmptyList expectedNonEmptyList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read (multiple event entries) with partition" {
                use testDir = new TestPersistenceDirectory<Counter>(Some "2026", None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                                $"""["EventJson",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","\"Incremented\""]]"""
                                $"""["EventJson",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser2Id.Guid}"],["Json","\"Incremented\""]]"""
                            ]
                        )

                    return! testDir.ReadAsync guid
                }

                match result with
                | Ok nonEmptyList ->
                    let expectedNonEmptyList =
                        NonEmptyList<Entry>
                            .Create(
                                EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                [
                                    EventJson(Rvn 2u, fixedUtcNow, auditUser1Id, (Incremented :> IEvent).EventJson)
                                    EventJson(Rvn 3u, fixedUtcNow, auditUser2Id, (Incremented :> IEvent).EventJson)
                                ]
                            )

                    Expect.equal nonEmptyList expectedNonEmptyList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read (multiple entries with snapshot) with no partition" {
                use testDir = new TestPersistenceDirectory<Counter>(None, Some 3u)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                                $"""["EventJson",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","\"Incremented\""]]"""
                                $"""["EventJson",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser2Id.Guid}"],["Json","\"Incremented\""]]"""
                                """["SnapshotJson",["Rvn",3],["Json","{\"Count\":1}"]]"""
                                $"""["EventJson",["Rvn",4],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","\"Incremented\""]]"""
                            ]
                        )

                    return! testDir.ReadAsync guid
                }

                match result with
                | Ok nonEmptyList ->
                    let expectedNonEmptyList =
                        NonEmptyList<Entry>
                            .Create(
                                SnapshotJson(Rvn 3u, ({ Count = 1 } :> IEntity).SnapshotJson),
                                [
                                    EventJson(Rvn 4u, fixedUtcNow, auditUser1Id, (Incremented :> IEvent).EventJson)
                                ]
                            )

                    Expect.equal nonEmptyList expectedNonEmptyList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all (no files) with partition" {
                use testDir = new TestPersistenceDirectory<Counter>(Some "2026", None)

                let! result = asyncResult { return! testDir.ReadAllAsync() }

                match result with
                | Ok list -> Expect.equal list [] $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all (initial event entry) with no partition" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    return! testDir.ReadAllAsync()
                }

                match result with
                | Ok list ->
                    let expectedList = [
                        Ok(
                            guid,
                            NonEmptyList<Entry>
                                .Create(
                                    EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                    []
                                )
                        )
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all (multiple event entries; multiple entries with snapshot) with partition" {
                use testDir = new TestPersistenceDirectory<Counter>(Some "2026", Some 3u)
                let guid1 = Guid.Empty // use empty Guid to ensure deterministic ordering of result
                let guid2 = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid1,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                                $"""["EventJson",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","\"Incremented\""]]"""
                            ]
                        )

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid2,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                                $"""["EventJson",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","\"Incremented\""]]"""
                                $"""["EventJson",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser2Id.Guid}"],["Json","\"Incremented\""]]"""
                                """["SnapshotJson",["Rvn",3],["Json","{\"Count\":1}"]]"""
                                $"""["EventJson",["Rvn",4],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","\"Incremented\""]]"""
                            ]
                        )

                    return! testDir.ReadAllAsync()
                }

                match result with
                | Ok list ->
                    let expectedList = [
                        Ok(
                            guid1,
                            NonEmptyList<Entry>
                                .Create(
                                    EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                    [
                                        EventJson(Rvn 2u, fixedUtcNow, auditUser1Id, (Incremented :> IEvent).EventJson)
                                    ]
                                )
                        )
                        Ok(
                            guid2,
                            NonEmptyList<Entry>
                                .Create(
                                    SnapshotJson(Rvn 3u, ({ Count = 1 } :> IEntity).SnapshotJson),
                                    [
                                        EventJson(Rvn 4u, fixedUtcNow, auditUser1Id, (Incremented :> IEvent).EventJson)
                                    ]
                                )
                        )
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Write (initial event entry) with no partition" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.WriteAsync(guid, Rvn.InitialRvn, Initialized -1, auditUser1Id, Json """{"Count":-1}""")

                    return! testDir.TryReadAllLinesAsync guid
                }

                match result with
                | Ok lines ->
                    let expectedLines = [
                        $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                    ]

                    Expect.equal lines expectedLines $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Write (multiple event entries) with partition" {
                use testDir = new TestPersistenceDirectory<Counter>(Some "2026", None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.WriteAsync(guid, Rvn.InitialRvn, Initialized -1, auditUser1Id, Json """{"Count":-1}""")

                    let! _ = testDir.WriteAsync(guid, Rvn 2u, Incremented, auditUser1Id, Json """{"Count":0}""")
                    let! _ = testDir.WriteAsync(guid, Rvn 3u, Incremented, auditUser2Id, Json """{"Count":1}""")

                    return! testDir.TryReadAllLinesAsync guid
                }

                match result with
                | Ok lines ->
                    let expectedLines = [
                        $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                        $"""["EventJson",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","\"Incremented\""]]"""
                        $"""["EventJson",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser2Id.Guid}"],["Json","\"Incremented\""]]"""
                    ]

                    Expect.equal lines expectedLines $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Write (initial event entry; multiple entries with snapshot) with no partition" {
                use testDir = new TestPersistenceDirectory<Counter>(None, Some 3u)
                let guid1, guid2 = Guid.NewGuid(), Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.WriteAsync(guid1, Rvn.InitialRvn, Initialized -1, auditUser1Id, Json """{"Count":-1}""")

                    let! _ =
                        testDir.WriteAsync(guid2, Rvn.InitialRvn, Initialized -1, auditUser1Id, Json """{"Count":-1}""")

                    let! _ = testDir.WriteAsync(guid2, Rvn 2u, Incremented, auditUser1Id, Json """{"Count":0}""")
                    let! _ = testDir.WriteAsync(guid2, Rvn 3u, Incremented, auditUser2Id, Json """{"Count":1}""")
                    let! _ = testDir.WriteAsync(guid2, Rvn 4u, Incremented, auditUser1Id, Json """{"Count":2}""")

                    let! lines1 = testDir.TryReadAllLinesAsync guid1
                    let! lines2 = testDir.TryReadAllLinesAsync guid2

                    return lines1, lines2
                }

                match result with
                | Ok(lines1, lines2) ->
                    let expectedLines1 = [
                        $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                    ]

                    let expectedLines2 = [
                        $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                        $"""["EventJson",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","\"Incremented\""]]"""
                        $"""["EventJson",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser2Id.Guid}"],["Json","\"Incremented\""]]"""
                        """["SnapshotJson",["Rvn",3],["Json","{\"Count\":1}"]]"""
                        $"""["EventJson",["Rvn",4],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","\"Incremented\""]]"""
                    ]

                    Expect.equal lines1 expectedLines1 $"Unexpected {nameof fst} {nameof Ok} {nameof result}"
                    Expect.equal lines2 expectedLines2 $"Unexpected {nameof snd} {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
        ]

    let private sad =
        testList "sad" [
            testAsync "Read when file does not exist" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult { return! testDir.ReadAsync guid }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"File does not exist when reading {guid} for {testDir.PathForError}"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Read when file exists but is empty" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ = testDir.TryWriteAllLinesAsync(guid, [])
                    return! testDir.ReadAsync guid
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"File exists but is empty when reading {guid} for {testDir.PathForError}"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Read when at least one entry caused a deserialization error" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                """["EventJason",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]""" // 'EventJason' should cause a deserialization error
                            ]
                        )

                    return! testDir.ReadAsync guid
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"At least one entry caused a deserialization error when reading {guid} for {testDir.PathForError} (e.g. Error at: `$`\010The following `failure` occurred with the decoder: Cannot find case EventJason in Aornota.Ubersweep.Server.Persistence.{nameof Entry})"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Read when event revision inconsistent with previous entry revision" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                """["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]"""
                                """["EventJson",["Rvn",3],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","\"Incremented\""]]"""
                            ]
                        )

                    return! testDir.ReadAsync guid
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"Consistency check failed when reading {guid} for {testDir.PathForError}: {nameof EventJson} with {Rvn 3u} inconsistent with previous {nameof Entry} ({Some(Rvn 1u)})"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Read when snapshot revision not equal to previous event revision" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                """["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]"""
                                """["SnapshotJson",["Rvn",2],["Json","{\"Count\":-1}"]]"""
                            ]
                        )

                    return! testDir.ReadAsync guid
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"Consistency check failed when reading {guid} for {testDir.PathForError}: {nameof SnapshotJson} with {Rvn 2u} not equal to previous {nameof Entry} ({Rvn 1u})"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Read when snapshot but previous entry was snapshot" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                """["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]"""
                                """["SnapshotJson",["Rvn",1],["Json","{\"Count\":-1}"]]"""
                                """["SnapshotJson",["Rvn",1],["Json","{\"Count\":-1}"]]"""
                            ]
                        )

                    return! testDir.ReadAsync guid
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"Consistency check failed when reading {guid} for {testDir.PathForError}: {nameof SnapshotJson} with {Rvn 1u} but previous {nameof Entry} was also {nameof SnapshotJson}"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Read when snapshot but no previous entry" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [ """["SnapshotJson",["Rvn",1],["Json","{\"Count\":2}"]]""" ]
                        )

                    return! testDir.ReadAsync guid
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"Consistency check failed when reading {guid} for {testDir.PathForError}: {nameof SnapshotJson} with {Rvn 1u} but no previous {nameof Entry}"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Read all when at least one file with non-Guid name exists" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let name = "rogue"

                let! result = asyncResult {
                    let! _ = testDir.TryWriteAllLinesAsync(name, [])
                    return! testDir.ReadAllAsync()
                }

                match result with
                | Ok list ->
                    let expectedList = [
                        Error
                            $"At least one .{FileReaderAndWriter.FileExtension} file in {testDir.PathForError} has a non-{nameof Guid} name (e.g. {name}.{FileReaderAndWriter.FileExtension})"
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all when error creating directory" {
                use testDir =
                    new TestPersistenceDirectory<Counter>(Some @"par|tition", None, skipCreatingDir = true)

                let! result = asyncResult { return! testDir.ReadAllAsync() }

                match result with
                | Ok list ->
                    let expectedList = [
                        Error
                            $"Error creating {testDir.PathForError} when reading all: The filename, directory name, or volume label syntax is incorrect. : '{testDir.Path}'"
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all when empty file exists" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guidOk = Guid.Empty // use empty Guid to ensure deterministic ordering of result
                let guidError = Guid.NewGuid()

                let! result = asyncResult {
                    // Create both valid and invalid files so can check result includes both.

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidOk,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    let! _ = testDir.TryWriteAllLinesAsync(guidError, [])

                    return! testDir.ReadAllAsync()
                }

                match result with
                | Ok list ->
                    let expectedList = [
                        Ok(
                            guidOk,
                            NonEmptyList<Entry>
                                .Create(
                                    EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                    []
                                )
                        )
                        Error $"File exists but is empty when reading {guidError} for {testDir.PathForError}"
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all when file exists for which at least one entry caused a deserialization error" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guidOk = Guid.Empty // use empty Guid to ensure deterministic ordering of result
                let guidError = Guid.NewGuid()

                let! result = asyncResult {
                    // Create both valid and invalid files so can check result includes both.

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidOk,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidError,
                            [
                                """["EventJason",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]""" // 'EventJason' should cause a deserialization error
                            ]
                        )

                    return! testDir.ReadAllAsync()
                }

                match result with
                | Ok list ->
                    let expectedList = [
                        Ok(
                            guidOk,
                            NonEmptyList<Entry>
                                .Create(
                                    EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                    []
                                )
                        )
                        Error
                            $"At least one entry caused a deserialization error when reading {guidError} for {testDir.PathForError} (e.g. Error at: `$`\010The following `failure` occurred with the decoder: Cannot find case EventJason in Aornota.Ubersweep.Server.Persistence.{nameof Entry})"
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all when file exists with event revision inconsistent with previous entry revision" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guidOk = Guid.Empty // use empty Guid to ensure deterministic ordering of result
                let guidError = Guid.NewGuid()

                let! result = asyncResult {
                    // Create both valid and invalid files so can check result includes both.

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidOk,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidError,
                            [
                                """["EventJson",["Rvn",2],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    return! testDir.ReadAllAsync()
                }

                match result with
                | Ok list ->
                    let expectedList = [
                        Ok(
                            guidOk,
                            NonEmptyList<Entry>
                                .Create(
                                    EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                    []
                                )
                        )
                        Error
                            $"Consistency check failed when reading {guidError} for {testDir.PathForError}: {nameof EventJson} with {Rvn 2u} inconsistent with previous {nameof Entry} ({None})"
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all when file exists with snapshot revision not equal to previous event revision" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guidOk = Guid.Empty // use empty Guid to ensure deterministic ordering of result
                let guidError = Guid.NewGuid()

                let! result = asyncResult {
                    // Create both valid and invalid files so can check result includes both.

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidOk,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidError,
                            [
                                """["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]"""
                                """["SnapshotJson",["Rvn",2],["Json","{\"Count\":-1}"]]"""
                            ]
                        )

                    return! testDir.ReadAllAsync()
                }

                match result with
                | Ok list ->
                    let expectedList = [
                        Ok(
                            guidOk,
                            NonEmptyList<Entry>
                                .Create(
                                    EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                    []
                                )
                        )
                        Error
                            $"Consistency check failed when reading {guidError} for {testDir.PathForError}: {nameof SnapshotJson} with {Rvn 2u} not equal to previous {nameof Entry} ({Rvn 1u})"
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all when file exists with snapshot but previous entry was snapshot" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guidOk = Guid.Empty // use empty Guid to ensure deterministic ordering of result
                let guidError = Guid.NewGuid()

                let! result = asyncResult {
                    // Create both valid and invalid files so can check result includes both.

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidOk,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidError,
                            [
                                """["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]"""
                                """["SnapshotJson",["Rvn",1],["Json","{\"Count\":-1}"]]"""
                                """["SnapshotJson",["Rvn",1],["Json","{\"Count\":-1}"]]"""
                            ]
                        )

                    return! testDir.ReadAllAsync()
                }

                match result with
                | Ok list ->
                    let expectedList = [
                        Ok(
                            guidOk,
                            NonEmptyList<Entry>
                                .Create(
                                    EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                    []
                                )
                        )
                        Error
                            $"Consistency check failed when reading {guidError} for {testDir.PathForError}: {nameof SnapshotJson} with {Rvn 1u} but previous {nameof Entry} was also {nameof SnapshotJson}"
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Read all when file exists with snapshot but no previous entry" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guidOk = Guid.Empty // use empty Guid to ensure deterministic ordering of result
                let guidError = Guid.NewGuid()

                let! result = asyncResult {
                    // Create both valid and invalid files so can check result includes both.

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidOk,
                            [
                                $"""["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","{auditUser1Id.Guid}"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guidError,
                            [ """["SnapshotJson",["Rvn",1],["Json","{\"Count\":2}"]]""" ]
                        )

                    return! testDir.ReadAllAsync()
                }

                match result with
                | Ok list ->
                    let expectedList = [
                        Ok(
                            guidOk,
                            NonEmptyList<Entry>
                                .Create(
                                    EventJson(Rvn 1u, fixedUtcNow, auditUser1Id, (Initialized -1 :> IEvent).EventJson),
                                    []
                                )
                        )
                        Error
                            $"Consistency check failed when reading {guidError} for {testDir.PathForError}: {nameof SnapshotJson} with {Rvn 1u} but no previous {nameof Entry}"
                    ]

                    Expect.equal list expectedList $"Unexpected {nameof Ok} {nameof result}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Write initial revision when file already exists" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ = testDir.TryWriteAllLinesAsync(guid, [])

                    return!
                        testDir.WriteAsync(guid, Rvn.InitialRvn, Initialized -1, auditUser1Id, Json """{"Count":-1}""")
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"File already exists when writing initial {Rvn.InitialRvn} for {guid} in {testDir.PathForError}"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Write non-initial revision when file does not exist" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    return! testDir.WriteAsync(guid, Rvn 2u, Incremented, auditUser1Id, Json """{"Count":0}""")
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"File does not exist when writing non-initial {Rvn 2u} for {guid} in {testDir.PathForError}"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Write non-initial revision when file exists but is empty" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ = testDir.TryWriteAllLinesAsync(guid, [])

                    return! testDir.WriteAsync(guid, Rvn 2u, Incremented, auditUser1Id, Json """{"Count":0}""")
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"File exists but is empty when writing non-initial {Rvn 2u} for {guid} in {testDir.PathForError}"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Write when revision inconsistent with previous entry revision" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                """["EventJson",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]"""
                            ]
                        )

                    return! testDir.WriteAsync(guid, Rvn 3u, Incremented, auditUser1Id, Json """{"Count":0}""")
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"Previous {nameof Entry} ({Rvn 1u}) not consistent when writing {Rvn 3u} for {guid} in {testDir.PathForError}"
                        $"{nameof Error} is not the expected error"
            }
            testAsync "Write when deserialization error for last entry" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)
                let guid = Guid.NewGuid()

                let! result = asyncResult {
                    let! _ =
                        testDir.TryWriteAllLinesAsync(
                            guid,
                            [
                                """["EventJason",["Rvn",1],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","[\"Initialized\",-1]"]]""" // 'EventJason' irrelevant as only the last entry will be deserialized
                                """["EventJson",["Revision",2],"2025-08-07T15:11:33.0000000Z",["Id","48edec54-5b2e-4ec9-afae-b554120ae856"],["Json","\"Incremented\""]]""" // 'Revision' should cause a deserialization error
                            ]
                        )

                    return! testDir.WriteAsync(guid, Rvn 3u, Incremented, auditUser1Id, Json """{"Count":0}""")
                }

                match result with
                | Ok _ -> Expect.isError result $"{nameof result} should be {nameof Error}"
                | Error error ->
                    Expect.equal
                        error
                        $"Deserialization error for last entry when writing {Rvn 3u} for {guid} in {testDir.PathForError}: Error at: `$[1]`\010The following `failure` occurred with the decoder: Cannot find case Revision in Aornota.Ubersweep.Shared.Rvn"
                        $"{nameof Error} is not the expected error"
            }
        ]

    let private integration =
        testList "integration" [
            testAsync "Write multiple entities (without snapshots) and read (separately) with no partition" {
                use testDir = new TestPersistenceDirectory<Counter>(None, None)

                let! result = asyncResult {
                    let guid1, guid2 = Guid.NewGuid(), Guid.NewGuid()

                    let! expected1 =
                        initializeAndApply (
                            guid1,
                            (Initialize -1, auditUser1Id),
                            [
                                Increment, auditUser1Id
                                Increment, auditUser1Id
                                Increment, auditUser1Id
                                MultiplyBy 2, auditUser2Id
                                Increment, auditUser1Id
                                MultiplyBy 2, auditUser2Id
                                Decrement, auditUser1Id
                            ],
                            testDir
                        )

                    let! entries1 = testDir.ReadAsync guid1
                    let! actual1 = Counter.eventHelper.FromEntries(guid1, entries1)

                    let! expected2 =
                        initializeAndApply (
                            guid2,
                            (Initialize 10, auditUser1Id),
                            [
                                DivideBy 5, auditUser1Id
                                Increment, auditUser1Id
                                Increment, auditUser1Id
                                MultiplyBy 3, auditUser2Id
                                Increment, auditUser1Id
                                MultiplyBy 2, auditUser2Id
                                Decrement, auditUser1Id
                            ],
                            testDir
                        )

                    let! entries2 = testDir.ReadAsync guid2
                    let! actual2 = Counter.eventHelper.FromEntries(guid2, entries2)

                    return actual1, expected1, actual2, expected2
                }

                match result with
                | Ok(actual1, expected1, actual2, expected2) ->
                    let expectedCount1, expectedCount2 = 9, 25

                    Expect.equal
                        expected1.State.Count
                        expectedCount1
                        $"Count for {nameof expected1} should equal {expectedCount1}"

                    Expect.equal
                        expected2.State.Count
                        expectedCount2
                        $"Count for {nameof expected2} should equal {expectedCount2}"

                    Expect.equal actual1 expected1 $"{nameof actual1} should equal {nameof expected1}"
                    Expect.equal actual2 expected2 $"{nameof actual2} should equal {nameof expected2}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
            testAsync "Write multiple entities (with snapshots) and read (all) with partition" {
                use testDir = new TestPersistenceDirectory<Counter>(Some "2026", Some 3u)

                let! result = asyncResult {
                    let guid1 = Guid.Empty // use empty Guid to ensure deterministic ordering of result
                    let guid2 = Guid.NewGuid()

                    let! expected1 =
                        initializeAndApply (
                            guid1,
                            (Initialize -1, auditUser1Id),
                            [
                                Increment, auditUser1Id
                                Increment, auditUser1Id
                                Increment, auditUser1Id
                                MultiplyBy 2, auditUser2Id
                                Increment, auditUser1Id
                                MultiplyBy 2, auditUser2Id
                                Decrement, auditUser1Id
                            ],
                            testDir
                        )

                    let! expected2 =
                        initializeAndApply (
                            guid2,
                            (Initialize 10, auditUser1Id),
                            [
                                DivideBy 5, auditUser1Id
                                Increment, auditUser1Id
                                Increment, auditUser1Id
                                MultiplyBy 3, auditUser2Id
                                Increment, auditUser1Id
                                MultiplyBy 2, auditUser2Id
                                Decrement, auditUser1Id
                            ],
                            testDir
                        )

                    let! all = testDir.ReadAllAsync()

                    let! result1, result2 =
                        match all with
                        | [ result1; result2 ] -> Ok(result1, result2)
                        | [] -> Error "Reading all returned no results"
                        | _ -> Error $"Reading all returned an unexpected number ({all.Length}) of results"

                    let! guidAndEntries1 = result1
                    let! actual1 = Counter.eventHelper.FromEntries guidAndEntries1

                    let! guidAndEntries2 = result2
                    let! actual2 = Counter.eventHelper.FromEntries guidAndEntries2

                    return actual1, expected1, actual2, expected2
                }

                match result with
                | Ok(actual1, expected1, actual2, expected2) ->
                    let expectedCount1, expectedCount2 = 9, 25

                    Expect.equal
                        expected1.State.Count
                        expectedCount1
                        $"Count for {nameof expected1} should equal {expectedCount1}"

                    Expect.equal
                        expected2.State.Count
                        expectedCount2
                        $"Count for {nameof expected2} should equal {expectedCount2}"

                    Expect.equal actual1 expected1 $"{nameof actual1} should equal {nameof expected1}"
                    Expect.equal actual2 expected2 $"{nameof actual2} should equal {nameof expected2}"
                | Error _ -> Expect.isOk result $"{nameof result} should be {nameof Ok}"
            }
        ]

    let tests = testList $"{nameof FileReaderAndWriter}" [ happy; sad; integration ]
