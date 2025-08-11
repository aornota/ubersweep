namespace Aornota.Ubersweep.Shared.Domain

type UserType =
    | SuperUser
    | Administrator
    | Pleb
    | PersonaNonGrata

type User = {
    // TODO: More fields...
    UserType: UserType
}
