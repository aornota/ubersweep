namespace Aornota.Ubersweep.Migration.Common

open Aornota.Ubersweep.Shared.Common

open Serilog

type SourcedLogger =
    [<Literal>]
    static let sourceContextPropertyName = "SourceContext"

    static member Create<'a>(logger: ILogger) =
        logger.ForContext(sourceContextPropertyName, $"{sanitize typeof<'a>}:")

    static member Create<'a>(details: string, logger: ILogger) =
        logger.ForContext(sourceContextPropertyName, $"{sanitize typeof<'a>} ({details}):")

    static member Create(details: string, logger: ILogger) =
        logger.ForContext(sourceContextPropertyName, $"{details}:")
