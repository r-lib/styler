# Validate sequence of new position ids

Ids created with `after = TRUE` can have `pos_id` values between x.0 and
x.5 and ids created with `after = FALSE` can have `pos_id` values
between 1+ x.0 and 1 + x.5 where x is the `pos_id` integer which was
used as a reference to create the new `pos_ids`.

## Usage

``` r
validate_new_pos_ids(new_ids, after)
```

## Arguments

- new_ids:

  A vector with new ids

- after:

  Whether the ids are created with `after = TRUE` (and hence should be
  in the range x.0-x.45) or not.

## See also

Other token creators:
[`create_pos_ids()`](https://styler.r-lib.org/reference/create_pos_ids.md),
[`create_tokens()`](https://styler.r-lib.org/reference/create_tokens.md)
