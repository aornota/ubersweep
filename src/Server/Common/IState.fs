namespace Aornota.Ubersweep.Server.Common

open Aornota.Ubersweep.Shared.Common

type IState<'state, 'event when 'state :> IState<'state, 'event> and 'event :> IEvent> =
    abstract SnapshotJson: Json
    abstract Evolve: 'event -> Result<'state, string>
