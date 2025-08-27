namespace Aornota.Ubersweep.Migration

open FsToolkit.ErrorHandling
open Serilog
open System.IO

(* TODO-MIGRATION: Remove this at some point - and:
    -- remove OutputType and AutoGenerateBindingRedirects properties from Migration.fsproj...
    -- remove WatchMigration target from Buiid.fs... *)

module Main =
    [<EntryPoint>]
    let main _ =
        Log.Logger <- LoggerConfiguration().MinimumLevel.Debug().WriteTo.Console().CreateLogger()

        let logger = Log.Logger

        let root = @"D:\UberSweep\persisted (legacy)"

        let readEverything () = asyncResult {
            let fifa2018 = Partition.fifa2018 (Path.Combine(root, "2018-fifa"), logger)

            let! _ = fifa2018.ReadDraftsAsync()
            let! _ = fifa2018.ReadFixturesAsync()
            let! _ = fifa2018.ReadPostsAsync()
            let! _ = fifa2018.ReadSquadsAsync()
            let! _ = fifa2018.ReadUsersAsync()
            let! _ = fifa2018.ReadUserDraftsAsync()

            let rwc2019 = Partition.rwc2019 (Path.Combine(root, "2019-rwc"), logger)

            let! _ = rwc2019.ReadDraftsAsync()
            let! _ = rwc2019.ReadFixturesAsync()
            let! _ = rwc2019.ReadPostsAsync()
            let! _ = rwc2019.ReadSquadsAsync()
            let! _ = rwc2019.ReadUsersAsync()
            let! _ = rwc2019.ReadUserDraftsAsync()

            let euro2020 = Partition.euro2020 (Path.Combine(root, "2020-euro"), logger)

            let! _ = euro2020.ReadDraftsAsync()
            let! _ = euro2020.ReadFixturesAsync()
            let! _ = euro2020.ReadPostsAsync()
            let! _ = euro2020.ReadSquadsAsync()
            let! _ = euro2020.ReadUsersAsync()
            let! _ = euro2020.ReadUserDraftsAsync()

            let fifa2022 = Partition.fifa2022 (Path.Combine(root, "2022-fifa"), logger)

            let! _ = fifa2022.ReadDraftsAsync()
            let! _ = fifa2022.ReadFixturesAsync()
            let! _ = fifa2022.ReadPostsAsync()
            let! _ = fifa2022.ReadSquadsAsync()
            let! _ = fifa2022.ReadUsersAsync()
            let! _ = fifa2022.ReadUserDraftsAsync()

            let rwc2023 = Partition.rwc2023 (Path.Combine(root, "2023-rwc"), logger)

            let! _ = rwc2023.ReadDraftsAsync()
            let! _ = rwc2023.ReadFixturesAsync()
            let! _ = rwc2023.ReadPostsAsync()
            let! _ = rwc2023.ReadSquadsAsync()
            let! _ = rwc2023.ReadUsersAsync()
            let! _ = rwc2023.ReadUserDraftsAsync()

            let euro2024 = Partition.euro2024 (Path.Combine(root, "2024-euro"), logger)

            let! _ = euro2024.ReadDraftsAsync()
            let! _ = euro2024.ReadFixturesAsync()
            let! _ = euro2024.ReadPostsAsync()
            let! _ = euro2024.ReadSquadsAsync()
            let! _ = euro2024.ReadUsersAsync()
            let! _ = euro2024.ReadUserDraftsAsync()

            return! Ok()
        }

        logger.Information "Reading everything..."

        let result = readEverything () |> Async.RunSynchronously

        match result with
        | Ok() ->
            logger.Information "...read everything without error"
            0
        | Error error ->
            logger.Error("...error when reading everything: {error}", error)
            -1
