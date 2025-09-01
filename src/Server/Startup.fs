namespace Aornota.Ubersweep.Server

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Server.Entities
open Aornota.Ubersweep.Server.Migration
open Aornota.Ubersweep.Server.Persistence
open Aornota.Ubersweep.Server.TEMP
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities
open Aornota.Ubersweep.Shared.TEMP

open FsToolkit.ErrorHandling
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
    let readUsersAsync (persistenceFactory: IPersistenceFactory, source: Source, logger: ILogger) = async {
        logger.Information("...reading {User}s...", nameof User)
        let reader = persistenceFactory.GetReader<User, UserEvent> None

        match! reader.ReadAllAsync() with
        | Ok pairs ->
            let users = pairs |> List.map User.helper.FromEntries |> List.sequenceResultA

            match users with
            | Ok list ->
                logger.Information("...{length} {User}/s read", list.Length, nameof User)
                let users = list |> List.map (fun user -> user.Id, user) |> Set.ofList

                return! async {
                    if
                        not (
                            users
                            |> Set.exists (fun (_, user) -> user.State.UserCommon.UserType = SuperUser)
                        )
                    then
                        logger.Warning(
                            "...creating {SuperUser} because no such {User}s exist...",
                            SuperUser,
                            nameof User
                        )

                        let initEvent =
                            UserCreated(
                                "superuser",
                                "CHQSX6YO/AM/Tm21txBUwSs5+8FcPFriq8HRKo7yDGA=",
                                "+eAhZRK85XUDQjEJ4HEwACNgCN607/BbfiWjcRjr4/WIyqGzMVhGlFtO7lhWSB9fwWzi4Nbzf74Kznm25WmSSw==",
                                SuperUser
                            )

                        let superUser =
                            User.helper.InitFromEvent(Guid "ffffffff-ffff-ffff-ffff-ffffffffffff", initEvent)

                        let writer = persistenceFactory.GetWriter<User, UserEvent> None

                        match! writer.WriteEventAsync(superUser.Guid, Rvn.InitialRvn, source, initEvent, None) with
                        | Ok() ->
                            logger.Information("...{SuperUser} created", SuperUser)
                            return Ok(users |> Set.add (superUser.Id, superUser))
                        | Error error ->
                            logger.Error("...error creating {SuperUser}: {error}", SuperUser, error)
                            return Error error
                    else
                        return Ok users
                }
            | Error errors -> return Error $"One or more error when processing entries for {nameof User}s: {errors}"
        | Error errors -> return Error $"One or more error when reading {nameof User}s: {errors}"
    }

    let readSweepstakesAsync (persistenceFactory: IPersistenceFactory, source: Source, logger: ILogger) = async {
        let typeToAutoCreate, yearToAutoCreate = Fifa, 2026u

        logger.Information("...reading {Sweepstake}s...", nameof Sweepstake)
        let reader = persistenceFactory.GetReader<Sweepstake, SweepstakeEvent> None

        match! reader.ReadAllAsync() with
        | Ok pairs ->
            let sweepstakes =
                pairs |> List.map Sweepstake.helper.FromEntries |> List.sequenceResultA

            match sweepstakes with
            | Ok list ->
                logger.Information("...{length} {Sweepstake}/s read", list.Length, nameof Sweepstake)
                let sweepstakes = list |> List.map (fun user -> user.Id, user) |> Set.ofList

                return! async {
                    if
                        not (
                            sweepstakes
                            |> Set.exists (fun (_, sweepstake) ->
                                sweepstake.State.SweepstakeCommon.SweepstakeType = typeToAutoCreate
                                && sweepstake.State.SweepstakeCommon.Year = yearToAutoCreate)
                        )
                    then
                        logger.Warning(
                            "...creating {typeToAutoCreate} {yearToAutoCreate} because no such {Sweepstake}s exist...",
                            typeToAutoCreate,
                            int yearToAutoCreate,
                            nameof Sweepstake
                        )

                        let initEvent =
                            SweepstakeCreated(
                                typeToAutoCreate,
                                yearToAutoCreate,
                                "The world-famous World Cup 2026 sweepstake",
                                // TODO-STARTUP: Create "logo"...
                                "https://github.com/aornota/ubersweep/blob/master/src/Client/public/sweepstake-2026-24x24.png",
                                26u,
                                Pending
                            )

                        let fifa2026 =
                            Sweepstake.helper.InitFromEvent(Guid "ffffffff-ffff-ffff-ffff-ffffffffffff", initEvent)

                        let writer = persistenceFactory.GetWriter<Sweepstake, SweepstakeEvent> None

                        match! writer.WriteEventAsync(fifa2026.Guid, Rvn.InitialRvn, source, initEvent, None) with
                        | Ok() ->
                            logger.Information(
                                "...{typeToAutoCreate} {yearToAutoCreate} created",
                                typeToAutoCreate,
                                int yearToAutoCreate
                            )

                            return Ok(sweepstakes |> Set.add (fifa2026.Id, fifa2026))
                        | Error error ->
                            logger.Error(
                                "...error creating {typeToAutoCreate} {yearToAutoCreate}: {error}",
                                typeToAutoCreate,
                                int yearToAutoCreate,
                                error
                            )

                            return Error error
                    else
                        return Ok sweepstakes
                }
            | Error errors ->
                return Error $"One or more error when processing entries for {nameof Sweepstake}s: {errors}"
        | Error errors -> return Error $"One or more error when reading {nameof Sweepstake}s: {errors}"
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

    do // run migration (subject to configuration)
        Migrator(config, persistenceFactory, logger).MigrateAsync()
        |> Async.RunSynchronously
        |> ignore<Result<unit, string>>

    // TODO-STARTUP: Create some sort of UserCache...
    let users =
        match
            Startup.readUsersAsync (persistenceFactory, System(nameof Startup), logger)
            |> Async.RunSynchronously
        with
        | Ok users -> users
        | Error error ->
            logger.Fatal("...error reading {User}s: {error}", nameof User, error)
            failwith $"Fatal error during {nameof Startup}: {error}"

    // TODO-STARTUP: Create some sort of SweepstakeCache...
    let sweepstakes =
        match
            Startup.readSweepstakesAsync (persistenceFactory, System(nameof Startup), logger)
            |> Async.RunSynchronously
        with
        | Ok sweepstakes -> sweepstakes
        | Error error ->
            logger.Fatal("...error reading {Sweepstake}s: {error}", nameof Sweepstake, error)
            failwith $"Fatal error during {nameof Startup}: {error}"

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
