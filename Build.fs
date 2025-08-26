open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

// TODO-BUILD: Add support for copying appsettings.production.json as appsettings.development.json if the latter does not exist?...

initializeContext ()

let sharedPath = Path.getFullName "src/Shared"
let serverPath = Path.getFullName "src/Server"
let clientPath = Path.getFullName "src/Client"
let migrationPath = Path.getFullName "src/Migration"
let deployPath = Path.getFullName "deploy"
let sharedTestsPath = Path.getFullName "tests/Shared"
let serverTestsPath = Path.getFullName "tests/Server"
let clientTestsPath = Path.getFullName "tests/Client"


Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet [ "fable"; "clean"; "--yes" ] clientPath // Delete *.fs.js files created by Fable
)

Target.create "RestoreClientDependencies" (fun _ -> run npm [ "ci" ] clientPath)

Target.create "Bundle" (fun _ ->
    [
        "server", dotnet [ "publish"; "-c"; "Release"; "-o"; deployPath ] serverPath
        "client", dotnet [ "fable"; "-o"; "output"; "-s"; "--run"; "npx"; "vite"; "build" ] clientPath
    ]
    |> runParallel)

Target.create "Azure" (fun _ ->
    let web = webApp {
        name "SAFE-App"
        operating_system OS.Linux
        runtime_stack (DotNet "8.0")
        zip_deploy "deploy"
    }

    let deployment = arm {
        location Location.WestEurope
        add_resource web
    }

    deployment
    |> Deploy.execute "SAFE-App" Deploy.NoParameters
    |> ignore<Map<string, string>>)

Target.create "Build" (fun _ -> run dotnet [ "build"; "ubersweep.sln" ] ".")

Target.create "Run" (fun _ ->
    [
        "server", dotnet [ "watch"; "run"; "--no-restore" ] serverPath
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ] clientPath
    ]
    |> runParallel)

Target.create "RunTestsHeadless" (fun _ ->
    run dotnet [ "run" ] serverTestsPath
    run npm [ "install" ] clientTestsPath
    run dotnet [ "fable"; "-o"; "output" ] clientTestsPath
    run npx [ "mocha"; "output" ] clientTestsPath)

Target.create "WatchRunTests" (fun _ ->
    [
        "server", dotnet [ "watch"; "run"; "--no-restore" ] serverTestsPath
        "client", dotnet [ "fable"; "watch"; "-o"; "output"; "-s"; "--run"; "npx"; "vite" ] clientTestsPath
    ]
    |> runParallel)

Target.create "Format" (fun _ -> run dotnet [ "fantomas"; "." ] ".")

Target.create "RunServerTests" (fun _ -> run dotnet [ "run" ] serverTestsPath) // TEMP

Target.create "WatchServer" (fun _ -> run dotnet [ "watch"; "run"; "--no-restore" ] serverPath) // TEMP

Target.create "WatchMigration" (fun _ -> run dotnet [ "watch"; "run"; "--no-restore" ] migrationPath) // TEMP

open Fake.Core.TargetOperators

let dependencies = [
    "Clean" ==> "RestoreClientDependencies" ==> "Bundle" ==> "Azure"
    "Clean" ==> "RestoreClientDependencies" ==> "Build" ==> "Run"

    "RestoreClientDependencies" ==> "Build" ==> "RunTestsHeadless"
    "RestoreClientDependencies" ==> "Build" ==> "WatchRunTests"
]

[<EntryPoint>]
let main args = runOrDefault args
