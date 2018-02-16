# General

In general, you would want to prioritize being consistent with the rest of the module.

In any record/data definition, the commas go on the Left Hand Side of the field.

# Database

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

Since we want our column names to be consistent with the columnn names we
put in the migration function, this seemed like the way to go.

## Naming tables in migration
In the module `MigrateScript`, please make sure the tables names are
`snake_case` as opposed to any variant of `camelCase`.

This is because in SQL queries, all captials are converted to small case, so
even if you name a table `myTable`, subsequent queries involving the table will
call your table `mytable`.

# Making Style Tweaks

If you want to make style tweaks to some modules
1. Ask if anyone else in the team is working on the module(s) in question
1. If not, make all the style tweaks within one commit.

# Notes
If you are unsure about a style tweak, please ask. If people cannot agree on
a style decision, please go with what the majority of the team find
comfortable and add it to this guide.

