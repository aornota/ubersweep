[<RequireQualifiedAccess>]
module Aornota.Ubersweep.Tests.Shared.TEMP.Shared.Tests

open Aornota.Ubersweep.Shared.TEMP

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let shared =
    testList "Shared" [
        testCase "Empty string is not a valid description"
        <| fun _ ->
            let expected = false
            let actual = Shared.isValid ""
            Expect.equal actual expected "Should be false"
    ]
