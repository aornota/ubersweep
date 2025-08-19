namespace Aornota.Ubersweep.Server.Entities

open Aornota.Ubersweep.Server.Common
open Aornota.Ubersweep.Shared.Common
open Aornota.Ubersweep.Shared.Entities

open FsToolkit.ErrorHandling
open System

type PostInitEvent =
    | PostCreated of userId: UserId * postType: PostType * messageText: Markdown * timestamp: DateTimeOffset

    interface IEvent with
        member this.EventJson = Json.toJson this

type PostEvent =
    | PostChanged of messageText: Markdown
    | PostRemoved

    interface IEvent with
        member this.EventJson = Json.toJson this

type Post = { // TODO: Implement this properly...
    Dummy: unit
} with

    interface IState<Post, PostEvent> with
        member this.SnapshotJson = Json.toJson this

        member this.Evolve event = this
