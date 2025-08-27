namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System
open Thoth.Json.Net

type PostInitEvent =
    | PostCreated of userId: UserId * postType: PostType * messageText: Markdown * timestamp: DateTimeOffset

    interface IEvent with
        member this.EventJson = Json.encode this

type PostEvent =
    | PostChanged of messageText: Markdown
    | PostRemoved

    interface IEvent with
        member this.EventJson = Json.encode this

type Post = {
    PostCommon: PostCommon'
    UserId: UserId
} with

    interface IState<Post, PostEvent> with
        member this.SnapshotJson = Json.encode this

        member this.Evolve event =
            match event with
            | PostChanged messageText ->
                Ok {
                    this with
                        PostCommon.MessageText = messageText
                }
            | PostRemoved -> Ok { this with PostCommon.Removed = true }

type PostHelper() =
    inherit EntityHelper<PostId, Post, PostInitCommand, PostInitEvent, PostEvent>()

    let eventDecoder =
        Decode.Auto.generateDecoderCached<PostEvent> (Json.caseStrategy, Json.extraCoders)

    let initEventDecoder =
        Decode.Auto.generateDecoderCached<PostInitEvent> (Json.caseStrategy, Json.extraCoders)

    let stateDecoder =
        Decode.Auto.generateDecoderCached<Post> (Json.caseStrategy, Json.extraCoders)

    override _.DecodeEvent(Json json) = Decode.fromString eventDecoder json
    override _.DecodeInitEvent(Json json) = Decode.fromString initEventDecoder json
    override _.DecodeState(Json json) = Decode.fromString stateDecoder json
    override _.IdFromGuid guid = PostId.FromGuid guid

    override _.InitFromCommand(guid, CreatePost(userId, postType, messageText, timestamp)) =
        {
            Id = PostId.FromGuid guid
            Rvn = Rvn.InitialRvn
            State = {
                PostCommon = {
                    PostType = postType
                    MessageText = messageText
                    Timestamp = timestamp
                    Removed = false
                }
                UserId = userId
            }
        },
        PostCreated(userId, postType, messageText, timestamp)

    override _.InitFromEvent(guid, PostCreated(userId, postType, messageText, timestamp)) = {
        Id = PostId.FromGuid guid
        Rvn = Rvn.InitialRvn
        State = {
            PostCommon = {
                PostType = postType
                MessageText = messageText
                Timestamp = timestamp
                Removed = false
            }
            UserId = userId
        }
    }

[<RequireQualifiedAccess>]
module Post =
    let private decide command (_: Post) =
        match command with
        | ChangePost messageText -> Ok(PostChanged messageText)
        | RemovePost -> Ok PostRemoved

    let helper = PostHelper()

    let apply command (entity: Entity<PostId, Post, PostEvent>) = result {
        let! event = decide command entity.State
        let! entity = entity.Evolve event
        return entity, event
    }
