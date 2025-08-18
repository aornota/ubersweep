namespace Aornota.Ubersweep.Migration.Common

open Serilog

type SourcedLogger =
    [<Literal>]
    static let sourceContextPropertyName = "SourceContext"

    static member Create<'a>(logger: ILogger) =
        logger.ForContext(sourceContextPropertyName, $"{typeof<'a>.Name}:")

    static member Create<'a>(details: string, logger: ILogger) =
        logger.ForContext(sourceContextPropertyName, $"{typeof<'a>.Name} ({details}):")

    static member Create(details: string, logger: ILogger) =
        logger.ForContext(sourceContextPropertyName, $"{details}:")
