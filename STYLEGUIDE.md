# General

1. In general, you would want to prioritize being consistent with the rest of the module.

1. In any record/data definition, the commas go on the Left Hand Side of the field.

1. Unless it's necessary(it would look hideous if we don't),
    we do not go over 80 characters in a line.
    In your editor, make sure there is some sort of mechanism to tell you
    whether or not you have crossed the 80 character limit
    Most good editors should allow you to have a ruler at the 81st character.

## Make sure code is hlint and -Wall clean before submitting PRs

Compile with `stack test --pedantic --fast` to make sure that the code is
"clean", and `hlint` should also be used either directly on the code or via your
editor. This doesn't necessarily need to be the case for all code pushed but
should be true of all code pushed to PRs once opened. This will eventually be
enforced in CI.

## Do not use unsafe functions

No:

* `head`
* `tail`
* `fromJust`
* `fromLeft/Right`
* `error`
* `fail` (unless it's How Things Are Done™ for that specific monad instance, eg:
    aeson parsers, and tests are usually fine)
* incomplete pattern matches (GHC will warn you)

## Use newtypes

Specific types are always better than general ones when defining data types,
instead of Text, make a newtype:

```haskell
newtype FooId = FooId Text deriving (...)
-- instead of
type FooId = Text
```

`newtypes` are preferred because they give extra type safety, type aliases are
exactly equal to the type which is being aliased so bugs from passing in the
wrong value are not detected at compile time.

## Default type classes to derive

For most data type definitions, at least `Show`, `Eq` and `Generic` should be
derived unless there is a good reason not to.

```haskell
{-# LANGUAGE DeriveGeneric #-}
...
import GHC.Generic
...
newtype EventWibble = EventWibble Text deriving (Show, Eq, Generic)
```

Doing this makes it much easier in many cases to derive instance of other
classes.

## Helper functions

A top-down approach to writing functions is preferred.
What that means is, helper functions
tend to go underneath the higher level function.

At the moment, this is restricted to function definitions,
not the function body. So, the use of `let` and `where`
is simply a matter of choice/consistency and
is not governed by this style rule.

If a helper function is being called by multiple higher-level functions,
then consider putting it in a relevant Utils module. Some examples of
Utils modules in this codebase are `QueryUtils`, `StorageUtils`, `Utils`.

If three or more of these helper functions pop up which you feel like need
a new module of their own, make one and name it `<Description>Utils.hs`.

## Database

We follow most of Beam's conventions.
There is only one we don't follow, which is mentioned later.

So, the table names are suffixed with a `T` (relative to the constructor).
Here is an example table definition:

```haskell
data MyTableT = MyTable
{
    column_one :: C f (Auto Int)
  , column_two :: PrimaryKey AnotherTable (Nullable f)
} deriving Generic
```

If you notice the field definitions (eg., `column_one`), it is in snake_case
as opposed to camelCase, which is unanimously conventional in Haskell.

The reason for that is that Beam looks at how you defined your columns (fields)
and [modifies](https://tathougies.github.io/beam/user-guide/models/#defaults)
the field name in the generated SQL query. At the time of writing, this
behaviour was found to be inconsistent and not well documented, and it was
discovered that `naming_variables_like_this_keeps_things_intact`.

Since we want our column names to be consistent with the column names we
put in the migration function, this seemed like the way to go.

### Database transactions

`Mirza.Common.Types` defines a `DB` monad, and some functions

```haskell
runDb ::  DBConstraint context err => DB context err a -> AppM context err a
pg    :: Pg a -> DB context err a
```

which will execute the given `DB` value within a transaction. Generally, any
single handler should only have a single call to `runDb` unless there is a good
reason why you _do not_ want transactional semantics.

```haskell
foo a b c = do
  thing <- getThing a
  runDb $ do
    -- Everything within this do block happens within a single database transaction
    users <- pg $ runSelectReturningList $ select $ do
      user <- all_ userTable
      guard_ (email_address user ==. val_ email)
      pure user
    things <- pg $ runSelectReturningList $ select $ do
        thing <- all_ thingTable
        guard_ (thingField ==. val athing)
        pure thing
    pg $ runInsertReturningList (otherThing theDB) $
        insertValues [...]
```

## Naming tables in migration

In the module `MigrateScript`, please make sure the tables names are
`snake_case` as opposed to any variant of `camelCase`.

This is because in SQL queries, all captials are converted to small case, so
even if you name a table `myTable`, subsequent queries involving the table will
call your table `mytable`.

## Making Style Tweaks

If you want to make style tweaks to some modules

1. Ask if anyone else in the team is working on the module(s) in question
1. If not, make all the style tweaks within one commit.

## Qualified imports

It is imperative that you make imports as explicit as possible, but also aim to
not clutter the course code too much. Most definitions defined within this
project can be imported without explicitly listing the imports or making them
qualified - this is our domain specific language. Modules from base or third
party libraries should always be imported using qualified names or explicit
import (or both, see below - this is very common in Haskell code bases).

If you only need to import small number of functions or types from a module and
they do not conflict with other imports, they may be imported explicitly by
name, for example:

```haskell
import Foo (Foo(..),toFoo, FooClass(..))
```

This should usually be preferred to importing qualified, as it is as explicit as
the qualified import, but without making the code more noisy by introducing
prefixes all over the place.

A common idiom used in Haskell code when for example a specific type is used a
lot in a module is to import it unqualified for those common types or functions,
and qualified with a short name for the rest of the module:

```haskell
import           Data.Text (Text, pack)
import qualified Data.Text as T
```

Modules this is commonly done for include:

* `Data.Text{.Lazy}` (qualified import as `T` and `TL`, importing `Text` unqualified)
* `Data.ByteString{.Lazy}` (qualified import as BS and BSL, importing `ByteString` unqualified)
* `Data.Map` (qualified import M, importing `Map` unqualified)
* `Data.HashMap` (qualified import HM, importing `HashMap` unqualified)

> Hint: The following snippet can be added to your Haskell language snippets in
> Visual Studio Code to make typing these imports easier (ask Alex if you need help):

```json
"Double import": {
        "prefix": "impq",
        "body": [
            "import           ${1:Data.Text} ($2)",
            "import qualified ${1:Data.Text} as ${3:Text}"
        ],
        "description": "Import a module qualified and unqualified"
    }
```

Some conventions:

1. `StorageBeam as SB`
1. `...SupplyChain.Types as ST`
1. `...Common.Types as CT`
1. `...Foo.Types as FT` etc.
1. `BeamQueries as BQ`
1. `Data.Text as T`
1. `Data.GS1.EPC as EPC`
1. `Data.GS1.Event as Ev`

## Notes

If you are unsure about a style tweak, please ask. If people cannot agree on
a style decision, please go with what the majority of the team find
comfortable and add it to this guide.

In general you should aim to make code _pretty_, by aligning elements textually
(using spaces) where it helps to see the larger structure of the program.
`stylish-haskell`, mentioned above, does some of this for you with record
definitions and case statements but it can aid greatly in code readability in
more cases than stylish-haskell handles. See the definition of `PrivateAPI` in
`API.hs` for an example.
