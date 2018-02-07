## Welcome!

To get started, it would be helpful to look at the
[GS1Combinators](https://github.csiro.au/Blockchain/GS1Combinators).

Afterwards, the Database library we are using is
[Beam](https://github.com/tathougies/beam).

If you haven't already or if you are not comfortable with Beam, it might be
worth completing the Beam tutorials, the first of which is
[here](https://tathougies.github.io/beam/tutorials/tutorial1/)

Our git workflow is [branch-based](https://guides.github.com/introduction/flow/)
(as opposed to being Fork based).

## Naming git branches
Please make sure your git branches have a name relevant to the work you are
doing in it suffixed by some form of identifier of your name (eg. your initials)

Making a branch is cheap (it is literally writing 40 characters to a file).
Please make branches freely so long as they make the git workflow more
understandable by everyone else in the team.

## Modules of interest
The entire migration function lives in the module `MigrateUtils`.
This module is very tightly coupled with `StorageBeam`. So when you change
something in the module `StorageBeam`, please make the equivalent change
in the `MigrateUtils` module as well.
