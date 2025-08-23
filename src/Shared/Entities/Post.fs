namespace Aornota.Ubersweep.Shared.Entities

open Aornota.Ubersweep.Shared.Common

open System

type PostId =
    private
    | PostId of guid: Guid

    static member Create() = PostId(Guid.NewGuid())
    static member FromGuid guid = PostId guid

    interface IId with
        member this.Guid =
            let (PostId guid) = this
            guid

type PostType =
    | Standard
    | MatchResult of fixtureId: FixtureId

type PostInitCommand =
    | CreatePost of userId: UserId * postType: PostType * messageText: Markdown * timestamp: DateTimeOffset

type PostCommand =
    | ChangePost of messageText: Markdown
    | RemovePost

type PostCommon' = {
    PostType: PostType
    MessageText: Markdown
    Timestamp: DateTimeOffset
    Removed: bool
}
