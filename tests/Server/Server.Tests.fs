module Aornota.Ubersweep.Server.Tests

open Aornota.Ubersweep.Shared
open Aornota.Ubersweep.Server
open Expecto

let server =
    testList "Server" [
        testCase "Adding valid Todo"
        <| fun _ ->
            let validTodo = Todo.create "TODO"
            let expectedResult = Ok()

            let result = Storage.addTodo validTodo

            Expect.equal result expectedResult "Result should be ok"
            Expect.contains Storage.todos validTodo "Storage should contain new todo"
    ]

let all = testList "All" [ Tests.shared; server ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all