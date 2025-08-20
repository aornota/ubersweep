namespace Aornota.Ubersweep.Server.Common

[<RequireQualifiedAccess>]
module AgentUser =
    // This is used as the audit UserId for events written without user interaction (e.g. DraftOpened and DraftPendingProcessing).
    [<Literal>]
    let agentUserGuid = "ffffffff-ffff-ffff-ffff-000000000001"
