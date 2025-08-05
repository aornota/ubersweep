module Aornota.Ubersweep.Client.Tests

open Aornota.Ubersweep.Index
open Aornota.Ubersweep.Shared
open Fable.Mocha
open SAFE

let client =
    testList "Client" [
        testCase "Added todo"
        <| fun _ ->
            let newTodo = Todo.create "new todo"
            let model, _ = init ()
            let model, _ = update (SaveTodo(Finished [ newTodo ])) model

            Expect.equal
                (model.Todos |> RemoteData.map _.Length |> RemoteData.defaultValue 0)
                1
                "There should be 1 todo"

            Expect.equal
                (model.Todos
                 |> RemoteData.map List.head
                 |> RemoteData.defaultValue (Todo.create ""))
                newTodo
                "Todo should equal new todo"
    ]

let all =
    testList "All" [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
        Tests.shared
#endif
        client
    ]

[<EntryPoint>]
let main _ = Mocha.runTests all