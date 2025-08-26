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
open Microsoft.Extensions.Hosting
open SAFE
open Serilog
open System
open System.Threading

module private Startup =
    [<Literal>]
    let private superUserGuid = "ffffffff-ffff-ffff-ffff-ffffffffffff"

    [<Literal>]
    let private superUserName = "superuser"

    [<Literal>]
    let private superUserPasswordSalt = "CHQSX6YO/AM/Tm21txBUwSs5+8FcPFriq8HRKo7yDGA="

    [<Literal>]
    let private superUserPasswordHash =
        "+eAhZRK85XUDQjEJ4HEwACNgCN607/BbfiWjcRjr4/WIyqGzMVhGlFtO7lhWSB9fwWzi4Nbzf74Kznm25WmSSw=="

    // TODO-STARTUP: Change this to create SuperUser if no SuperUsers exist? And to populate "UserCache"? And do something similar for Fifa-2026 Sweepstake?...
    let checkUsersAsync (persistenceFactory: IPersistenceFactory, source: Source, logger: ILogger) = async {
        logger.Information("...checking {User}s...", nameof User)
        let reader = persistenceFactory.GetReader<User, UserEvent> None
        let! result = reader.ReadAllAsync()

        match result with
        | Ok [] ->
            logger.Information("...creating {SuperUser} because no {User}s exist...", SuperUser, nameof User)

            let initEvent =
                UserCreated(superUserName, superUserPasswordSalt, superUserPasswordHash, SuperUser)

            let superUser = User.helper.InitFromEvent(Guid superUserGuid, initEvent)
            let writer = persistenceFactory.GetWriter<User, UserEvent> None

            let! result =
                writer.WriteEventAsync(
                    superUser.Guid,
                    Rvn.InitialRvn,
                    source,
                    initEvent,
                    Some(fun _ -> superUser.SnapshotJson)
                )

            match result with
            | Ok() -> logger.Information("...{SuperUser} created", SuperUser)
            | Error error -> logger.Error("...error creating {SuperUser}: {error}", SuperUser, error)

        (* TODO-PERSISTENCE: Implement ReadAsync?...
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
            *)
        | Ok list ->
            list
            |> List.iter (fun (guid, entries) ->
                match User.helper.FromEntries(guid, entries) with
                | Ok _ -> ()
                | Error error ->
                    logger.Error("...error processing {User} entries for {guid}: {error}", nameof User, guid, error))

            logger.Information("...{length} {User}/s checked", list.Length, nameof User)
        | Error errors -> logger.Error("...errors checking {User}s: {errors}", nameof User, errors)
    }

type Startup(config) =
    do // configure logging
        Log.Logger <-
            LoggerConfiguration()
                .ReadFrom.Configuration(config)
                .Destructure.FSharpTypes()
                .CreateLogger()

    let logger = SourcedLogger.Create<Startup> Log.Logger

    do logger.Information "Starting..."

    let persistenceFactory =
        new FilePersistenceFactory(config, PersistenceClock(), logger)

    do // run migration (if Migrate:MigrateOnStartup setting is true)
        Migration(config, persistenceFactory, logger).MigrateAsync()
        |> Async.RunSynchronously
        |> ignore<Result<unit, string list>>

    // TODO-STARTUP...do Startup.checkUsersAsync (persistenceFactory, System (nameof Startup), logger) |> Async.RunSynchronously

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

    member __.Configure(builder: IApplicationBuilder, hostApplicationLifetime: IHostApplicationLifetime) =
        hostApplicationLifetime.ApplicationStopped.Register(fun _ -> (persistenceFactory :> IDisposable).Dispose())
        |> ignore<CancellationTokenRegistration>

        builder.UseFileServer().UseGiraffe webApp

    member __.ConfigureServices(services: IServiceCollection) =
        services.AddGiraffe().AddSingleton(persistenceFactory).AddSingleton(Log.Logger)
        |> ignore<IServiceCollection>
