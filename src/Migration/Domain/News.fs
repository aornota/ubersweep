namespace Aornota.Ubersweep.Migration.Domain

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Shared.Common

open System

type PostId =
    | PostId of guid: Guid

    static member Create() = Guid.NewGuid() |> PostId

type PostType =
    | Standard
    | MatchResult of fixtureId: FixtureId

type PostTypeDto =
    | StandardDto of messageText: Markdown
    | MatchResultDto of messageText: Markdown * fixtureId: FixtureId

type PostDto = {
    PostId: PostId
    Rvn: Rvn
    UserId: UserId
    PostTypeDto: PostTypeDto
    Timestamp: DateTimeOffset
}
