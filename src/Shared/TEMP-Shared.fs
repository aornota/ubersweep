namespace Aornota.Ubersweep.Shared.TEMP

open System

type Todo = { Id: Guid; Description: string }

[<RequireQualifiedAccess>]
module Shared =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) = {
        Id = Guid.NewGuid()
        Description = description
    }

    type ITodosApi = {
        getTodos: unit -> Async<Todo list>
        addTodo: Todo -> Async<Todo list>
    }
