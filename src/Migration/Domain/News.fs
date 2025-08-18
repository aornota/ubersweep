namespace Aornota.Ubersweep.Migration.Domain

type PostType =
    | Standard
    | MatchResult of fixtureId: FixtureId
