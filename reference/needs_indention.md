# Check whether indention is needed

Checks for each potential trigger token in `pd` whether it actually
should cause indention.

## Usage

``` r
needs_indention(pd, potential_triggers_pos, other_trigger_tokens = NULL)
```

## Arguments

- pd:

  A parse table.

- potential_triggers_pos:

  A vector with indices of the potential trigger tokens in `pd`.

- other_trigger_tokens:

  Other tokens that are going to cause indention if on the same line as
  the token corresponding to `potential_trigger` and directly followed
  by a line break.
