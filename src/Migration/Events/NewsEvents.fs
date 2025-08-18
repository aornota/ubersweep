namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Migration.Common
open Aornota.Ubersweep.Migration.Domain

open System

type NewsEvent =
    | PostCreated of
        postId: PostId *
        userId: UserId *
        postType: PostType *
        messageText: Markdown *
        timestamp: DateTimeOffset
    | PostChanged of postId: PostId * messageText: Markdown
    | PostRemoved of postId: PostId
