# Instructions for Claude

## Keep the documentation up to date with every change

Every change to this repository must update all the documentation it affects,
in the same commit or pull request. A change is not finished until its
documentation is. Before you finish, check each of these and update what the
change touches:

- `README.md`: modules, examples and caveats. Code examples must still
  compile against the new code, and version numbers in examples must name the
  current release.
- `CHANGELOG.md`: describe the change under the next version.
- Haddock in `src`: add Haddock for every new declaration, and fix comments
  that the change made wrong.

This package defines the wire format of Haskoin Store, which other programs
decode. A change to a JSON or binary encoding, or to an exported name, breaks
them: say so in the changelog, and keep old names working by re-exporting
them where possible. The swagger API reference lives in the haskoin-store
repository and must be updated there when an encoding changes.

If a change needs no documentation update, say so explicitly when you report
the change, with the reason.

## How to write documentation here

Documentation explains the purpose and spirit of the code, not only its
behaviour. For every module and every significant type or function, say what
problem it solves and why it is designed the way it is, including the
trade-offs and the alternatives it rejects. A reader should come away knowing
when to use it and when not to. Keep to the structure the README already has,
and put new material in the section where a reader would look for it.

## Checks

Build the documentation with `stack haddock --no-haddock-deps` and make sure it
has no warnings about broken markup. CI builds it too, both with the OS
libraries and with `stack --nix`. Format with `scripts/format` (ormolu).
