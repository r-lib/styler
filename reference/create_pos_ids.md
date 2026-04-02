# Create valid pos_ids if possible

Create valid pos_ids if possible

## Usage

``` r
create_pos_ids(pd, pos, by = 0.1, after = FALSE, n = 1L)
```

## Arguments

- pd:

  A parse table.

- pos:

  The position where the new id should be inserted.

- by:

  By how much the reference `pos_id` should be increased / decreased to
  create a new id.

- after:

  Boolean indicating whether it should be inserted after or before
  `pos`.

- n:

  Number of ids to generate.

## Value

Returns a valid sequences of pos_ids or an error if it was not possible
to create one. The validation is done with
[`validate_new_pos_ids()`](https://styler.r-lib.org/reference/validate_new_pos_ids.md)

## See also

Other token creators:
[`create_tokens()`](https://styler.r-lib.org/reference/create_tokens.md),
[`validate_new_pos_ids()`](https://styler.r-lib.org/reference/validate_new_pos_ids.md)
