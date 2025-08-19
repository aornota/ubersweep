namespace Aornota.Ubersweep.Migration.Common

open Aornota.Ubersweep.Shared.Common

type IHelper<'event, 'entity> =
    abstract member ApplyEvents: 'event list -> Result<'entity * Rvn, string>
