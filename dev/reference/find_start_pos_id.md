# Find legit starting value for a new positional id

Looks at the current nest as well as into its children (if necessary) to
make sure the right id is returned. Otherwise, ordering of tokens might
not be preserved.

## Usage

``` r
find_start_pos_id(pd, pos, by, direction, after, candidates = NULL)
```

## Arguments

- pd:

  A parse table.

- pos:

  The position where the new id should be inserted.

- by:

  By how much the reference `pos_id` should be increased / decreased to
  create a new id.

- direction:

  Derived from `after`. `1` if `after = TRUE`, `-1` otherwise.

- after:

  Boolean indicating whether it should be inserted after or before
  `pos`.

- candidates:

  The `pos_ids` of the candidates that origin from other nests.
