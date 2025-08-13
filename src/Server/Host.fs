namespace Aornota.Ubersweep.Server

open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Serilog

module Host =
    let private configuration =
        ConfigurationBuilder()
            .AddJsonFile("appsettings.json", false)
#if DEBUG
            .AddJsonFile("appsettings.development.json", false)
#else
            .AddJsonFile("appsettings.production.json", false)
#endif
            .Build()

    [<EntryPoint>]
    let main _ =
        Host
            .CreateDefaultBuilder()
            .ConfigureWebHostDefaults(fun builder ->
                builder
                    .UseConfiguration(configuration)
                    .ConfigureLogging(fun builder -> builder.ClearProviders().AddSerilog() |> ignore)
                    //.ConfigureLogging(fun builder -> builder.AddSerilog() |> ignore)
                    .UseStartup<Startup>()
                    .UseWebRoot("public")
                |> ignore)
            .Build()
            .Run()

        0
