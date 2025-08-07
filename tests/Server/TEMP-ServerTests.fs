namespace Aornota.Ubersweep.Tests.Server.TEMP

open Aornota.Ubersweep.Server.TEMP
open Aornota.Ubersweep.Shared.TEMP
open Aornota.Ubersweep.Tests.Shared.TEMP

open Expecto

module ServerTests =
    let server =
        testList "Server" [
            testCase "Adding valid Todo"
            <| fun _ ->
                let validTodo = Shared.create "TODO"
                let expectedResult = Ok()

                let result = Storage.addTodo validTodo

                Expect.equal result expectedResult "Result should be ok"
                Expect.contains Storage.todos validTodo "Storage should contain new todo"
        ]

    let all = testList "All" [ SharedTests.shared; server ]

    [<EntryPoint>]
    let main _ = runTestsWithCLIArgs [] [||] all
