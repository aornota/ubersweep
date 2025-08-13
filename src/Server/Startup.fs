namespace Aornota.Ubersweep.Server

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Server.TEMP
open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Shared.Domain.Entities
open Aornota.Ubersweep.Shared.TEMP

open Giraffe
open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open SAFE
open Serilog
open System

type Startup(config) =
    do
        Log.Logger <-
            LoggerConfiguration()
                .ReadFrom.Configuration(config)
                .Destructure.FSharpTypes()
                .CreateLogger()

    let logger = SourcedLogger.Create<Startup> Log.Logger

    do logger.Information "Started"

    (* TEMP: To check logging...
    let persistenceFactory =
        FilePersistenceFactory(config, PersistenceClock(), logger) :> IPersistenceFactory

    let reader = persistenceFactory.GetReader<User> None

    let writer = persistenceFactory.GetWriter<User> None

    let _ = reader.ReadAllAsync() |> Async.RunSynchronously

    let _ = reader.ReadAsync(Guid.NewGuid()) |> Async.RunSynchronously
    let _ = reader.ReadAsync Guid.Empty |> Async.RunSynchronously

    let _ =
        writer.WriteAsync(
            Guid.NewGuid(),
            Rvn.InitialRvn,
            EntityId<User>.Create(),
            Json "InitEvent",
            (fun _ -> Json "Snapshot")
        )
        |> Async.RunSynchronously
    *)

    let todosApi ctx = {
        Shared.getTodos = fun () -> async { return Storage.todos |> List.ofSeq }
        Shared.addTodo =
            fun todo -> async {
                return
                    match Storage.addTodo todo with
                    | Ok() -> Storage.todos |> List.ofSeq
                    | Error e -> failwith e
            }
    }

    let webApp = Api.make todosApi |> SerilogAdapter.Enable

    member __.Configure(builder: IApplicationBuilder) =
        builder.UseFileServer().UseGiraffe webApp

    member __.ConfigureServices(services: IServiceCollection) =
        services
            .AddGiraffe()
            .AddSingleton(FilePersistenceFactory)
            .AddSingleton(Log.Logger)
        |> ignore
