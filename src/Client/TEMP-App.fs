namespace Aornota.Ubersweep.Client.TEMP

open Elmish
open Fable.Core.JsInterop

module App =
    importSideEffects "./index.css"

#if DEBUG
    open Elmish.HMR
#endif

    Program.mkProgram Index.init Index.update Index.view
#if DEBUG
    |> Program.withConsoleTrace
#endif
    |> Program.withReactSynchronous "elmish-app"
    |> Program.run
