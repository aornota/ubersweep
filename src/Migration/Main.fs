namespace Aornota.Ubersweep.Migration

open Serilog
open System.IO

module Main =
    [<EntryPoint>]
    let main _ =
        Log.Logger <- LoggerConfiguration().MinimumLevel.Debug().WriteTo.Console().CreateLogger()

        let logger = Log.Logger

        let root = @"D:\UberSweep\persisted (legacy)"

        let fifa2018 = Partition.fifa2018 (Path.Combine(root, "2018-fifa"), logger)

        fifa2018.ReadDrafts() |> ignore<Result<_, _>>
        fifa2018.ReadFixtures() |> ignore<Result<_, _>>
        fifa2018.ReadPosts() |> ignore<Result<_, _>>
        fifa2018.ReadSquads() |> ignore<Result<_, _>>
        fifa2018.ReadUsers() |> ignore<Result<_, _>>
        fifa2018.ReadUserDrafts() |> ignore<Result<_, _>>

        let rwc2019 = Partition.rwc2019 (Path.Combine(root, "2019-rwc"), logger)

        rwc2019.ReadDrafts() |> ignore<Result<_, _>>
        rwc2019.ReadFixtures() |> ignore<Result<_, _>>
        rwc2019.ReadPosts() |> ignore<Result<_, _>>
        rwc2019.ReadSquads() |> ignore<Result<_, _>>
        rwc2019.ReadUsers() |> ignore<Result<_, _>>
        rwc2019.ReadUserDrafts() |> ignore<Result<_, _>>

        let euro2021 = Partition.euro2021 (Path.Combine(root, "2021-euro"), logger)

        euro2021.ReadDrafts() |> ignore<Result<_, _>>
        euro2021.ReadFixtures() |> ignore<Result<_, _>>
        euro2021.ReadPosts() |> ignore<Result<_, _>>
        euro2021.ReadSquads() |> ignore<Result<_, _>>
        euro2021.ReadUsers() |> ignore<Result<_, _>>
        euro2021.ReadUserDrafts() |> ignore<Result<_, _>>

        let fifa2022 = Partition.fifa2022 (Path.Combine(root, "2022-fifa"), logger)

        fifa2022.ReadDrafts() |> ignore<Result<_, _>>
        fifa2022.ReadFixtures() |> ignore<Result<_, _>>
        fifa2022.ReadPosts() |> ignore<Result<_, _>>
        fifa2022.ReadSquads() |> ignore<Result<_, _>>
        fifa2022.ReadUsers() |> ignore<Result<_, _>>
        fifa2022.ReadUserDrafts() |> ignore<Result<_, _>>

        let rwc2023 = Partition.rwc2023 (Path.Combine(root, "2023-rwc"), logger)

        rwc2023.ReadDrafts() |> ignore<Result<_, _>>
        rwc2023.ReadFixtures() |> ignore<Result<_, _>>
        rwc2023.ReadPosts() |> ignore<Result<_, _>>
        rwc2023.ReadSquads() |> ignore<Result<_, _>>
        rwc2023.ReadUsers() |> ignore<Result<_, _>>
        rwc2023.ReadUserDrafts() |> ignore<Result<_, _>>

        let euro2024 = Partition.euro2024 (Path.Combine(root, "2024-euro"), logger)

        euro2024.ReadDrafts() |> ignore<Result<_, _>>
        euro2024.ReadFixtures() |> ignore<Result<_, _>>
        euro2024.ReadPosts() |> ignore<Result<_, _>>
        euro2024.ReadSquads() |> ignore<Result<_, _>>
        euro2024.ReadUsers() |> ignore<Result<_, _>>
        euro2024.ReadUserDrafts() |> ignore<Result<_, _>>

        0
