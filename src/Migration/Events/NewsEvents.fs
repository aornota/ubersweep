namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain
open Aornota.Ubersweep.Shared.Common

open System

type PostId =
    | PostId of guid: Guid

    static member Create() = Guid.NewGuid() |> PostId

type NewsEvent =
    | PostCreated of
        postId: PostId *
        userId: UserId *
        postType: PostType *
        messageText: Markdown *
        timestamp: DateTimeOffset
    | PostChanged of postId: PostId * messageText: Markdown
    | PostRemoved of postId: PostId

type Post = {
    UserId: UserId
    PostType: PostType
    MessageText: Markdown
    Timestamp: DateTimeOffset
    Removed: bool
}

type NewsHelper() =
    let rec applyEvents events postAndRvn =
        match postAndRvn, events with
        | None, PostCreated(_, userId, postType, messageText, timestamp) :: t ->
            applyEvents
                t
                (Some(
                    {
                        UserId = userId
                        PostType = postType
                        MessageText = messageText
                        Timestamp = timestamp
                        Removed = false
                    },
                    Rvn.InitialRvn
                ))
        | Some(post, rvn), PostChanged(_, messageText) :: t ->
            applyEvents t (Some({ post with MessageText = messageText }, rvn.NextRvn))
        | Some(post, rvn), PostRemoved _ :: t -> applyEvents t (Some({ post with Removed = true }, rvn.NextRvn))
        | Some postAndRvn, [] -> Ok postAndRvn
        | None, [] -> Error $"No initial {nameof NewsEvent}"
        | None, h :: _ -> Error $"Invalid initial {nameof NewsEvent}: {h}"
        | Some _, PostCreated _ :: _ -> Error $"Invalid non-initial {nameof NewsEvent}: {nameof PostCreated}"

    interface IHelper<NewsEvent, Post> with
        member _.ApplyEvents events = applyEvents events None
