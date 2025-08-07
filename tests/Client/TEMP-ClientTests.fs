namespace Aornota.Ubersweep.Tests.Client.TEMP

open Aornota.Ubersweep.Client.TEMP
open Aornota.Ubersweep.Shared.TEMP

open Fable.Mocha
open SAFE

module ClientTests =
    let client =
        testList "Client" [
            testCase "Added todo"
            <| fun _ ->
                let newTodo = Shared.create "new todo"
                let model, _ = Index.init ()
                let model, _ = Index.update (SaveTodo(Finished [ newTodo ])) model

                Expect.equal
                    (model.Todos |> RemoteData.map _.Length |> RemoteData.defaultValue 0)
                    1
                    "There should be 1 todo"

                Expect.equal
                    (model.Todos
                     |> RemoteData.map List.head
                     |> RemoteData.defaultValue (Shared.create ""))
                    newTodo
                    "Todo should equal new todo"
        ]

    let all =
        testList "All" [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
            Aornota.Ubersweep.Tests.Shared.TEMP.SharedTests.shared
#endif
            client
        ]

    [<EntryPoint>]
    let main _ = Mocha.runTests all
