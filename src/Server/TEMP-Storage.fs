[<RequireQualifiedAccess>]
module Aornota.Ubersweep.Server.TEMP.Storage

open Aornota.Ubersweep.Shared.TEMP

let todos =
    ResizeArray [
        Shared.create "Create new SAFE project"
        Shared.create "Write your app"
        Shared.create "Ship it!!!"
    ]

let addTodo (todo: Shared.Todo) =
    if Shared.isValid todo.Description then
        todos.Add todo
        Ok()
    else
        Error "Invalid todo"
