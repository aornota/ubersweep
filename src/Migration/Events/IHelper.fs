namespace Aornota.Ubersweep.Migration.Events

open Aornota.Ubersweep.Shared.Common

type IHelper<'event, 'entity> =
    abstract member ApplyEvents: 'event list -> Result<'entity * Rvn, string>
