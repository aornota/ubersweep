namespace Aornota.Ubersweep.Server.TEMP

open Aornota.Ubersweep.Shared.TEMP

[<RequireQualifiedAccess>]
module Storage =
    let todos =
        ResizeArray [
            Shared.create "Create new SAFE project"
            Shared.create "Write your app"
            Shared.create "Ship it!!!"
        ]

    let addTodo (todo: Todo) =
        if Shared.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"
