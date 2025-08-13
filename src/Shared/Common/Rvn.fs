namespace Aornota.Ubersweep.Shared.Common

type Rvn =
    | Rvn of rvn: uint

    static member InitialRvn = Rvn 1u
    member this.IsInitialRvn = this = Rvn.InitialRvn

    member this.NextRvn =
        let (Rvn rvn) = this
        Rvn(rvn + 1u)

    member this.IsValidNextRvn(currentRvn: Rvn option) =
        match currentRvn with
        | None when this.IsInitialRvn -> true
        | Some currentRvn when currentRvn.NextRvn = this -> true
        | _ -> false
