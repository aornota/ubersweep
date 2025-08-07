namespace Aornota.Ubersweep.Shared

type NonEmptyList<'a> = private {
    Head': 'a
    Tail': 'a list
} with

    static member Create(head, tail) = { Head' = head; Tail' = tail }

    static member FromList(list: 'a list) =
        match list with
        | h :: t -> Ok(NonEmptyList<'a>.Create(h, t))
        | [] -> Error $"{nameof list} is empty"

    member this.Head = this.Head'
    member this.Tail = this.Tail'
    member this.List = this.Head' :: this.Tail'
