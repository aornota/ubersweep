namespace Aornota.Ubersweep.Server

open Aornota.Ubersweep.Server.TEMP
open Aornota.Ubersweep.Shared.TEMP

open SAFE
open Saturn

module Main =
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

    let webApp = Api.make todosApi

    let app = application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

    [<EntryPoint>]
    let main _ =
        run app
        0
