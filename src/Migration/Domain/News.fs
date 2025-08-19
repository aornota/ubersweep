namespace Aornota.Ubersweep.Migration.Domain

open System

type PostId =
    | PostId of guid: Guid

    static member Create() = Guid.NewGuid() |> PostId

type PostType =
    | Standard
    | MatchResult of fixtureId: FixtureId
