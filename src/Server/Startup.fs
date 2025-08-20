namespace Aornota.Ubersweep.Server

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Server.Migration
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Server.TEMP
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities
open Aornota.Ubersweep.Shared.TEMP

open Giraffe
open Giraffe.SerilogExtensions
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open SAFE
open Serilog
open System

module private Startup =
    [<Literal>]
    let private SuperUserName = "superuser"

    [<Literal>]
    let private SuperUserPasswordSalt = "CHQSX6YO/AM/Tm21txBUwSs5+8FcPFriq8HRKo7yDGA="

    [<Literal>]
    let private SuperUserPasswordHash =
        "+eAhZRK85XUDQjEJ4HEwACNgCN607/BbfiWjcRjr4/WIyqGzMVhGlFtO7lhWSB9fwWzi4Nbzf74Kznm25WmSSw=="

    let checkUsersAsync (persistenceFactory: IPersistenceFactory, logger: ILogger) = async {
        logger.Information "...checking Users..."

        let reader = persistenceFactory.GetReader<User, UserEvent> None
        let! all = reader.ReadAllAsync()

        if all.Length > 0 then
            all
            |> List.iter (fun result ->
                match result with
                | Ok(guid, entries) ->
                    match User.helper.FromEntries(guid, entries) with
                    | Ok _ -> ()
                    | Error error -> logger.Error("...error processing entries for User {guid}: {error}", guid, error)
                | Error error -> logger.Error("...error reading entries for User: {error}", error))

            logger.Information("...{length} User/s checked", all.Length)
        else
            logger.Information("...creating {type} because no Users exist...", SuperUser)

            let initEvent =
                UserCreated(SuperUserName, SuperUserPasswordSalt, SuperUserPasswordHash, SuperUser)

            let superUser = User.helper.InitFromEvent(Guid.Empty, initEvent)

            let writer = persistenceFactory.GetWriter<User, UserEvent> None

            let! result =
                writer.WriteEventAsync(
                    superUser.Guid,
                    Rvn.InitialRvn,
                    superUser.Id,
                    initEvent,
                    Some(fun _ -> superUser.SnapshotJson)
                )

            match result with
            | Ok _ ->
                match! reader.ReadAsync superUser.Guid with
                | Ok entries ->
                    match User.helper.FromEntries(superUser.Guid, entries) with
                    | Ok _ -> logger.Information("...{type} created", SuperUser)
                    | Error error ->
                        logger.Error(
                            "...error processing entries for {type} {guid}: {error}",
                            SuperUser,
                            superUser.Guid,
                            error
                        )
                | Error error -> logger.Error("...error reading entries for {type}: {error}", SuperUser, error)
            | Error error -> logger.Error("...error creating {type}: {error}", SuperUser, error)

        ()
    }

type Startup(config) =
    do
        Log.Logger <-
            LoggerConfiguration()
                .ReadFrom.Configuration(config)
                .Destructure.FSharpTypes()
                .CreateLogger()

    let logger = SourcedLogger.Create<Startup> Log.Logger

    do logger.Information "Starting..."

    let persistenceFactory =
        FilePersistenceFactory(config, PersistenceClock(), logger) :> IPersistenceFactory

    do
        Migration(config, persistenceFactory, logger).MigrateAsync()
        |> Async.RunSynchronously
        |> ignore

    // TEMP...do Startup.checkUsersAsync (persistenceFactory, logger) |> Async.RunSynchronously

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
        services.AddGiraffe().AddSingleton(persistenceFactory).AddSingleton(Log.Logger)
        |> ignore
