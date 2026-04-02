# Check whether indention is needed

Determine whether the tokens corresponding to `potential_trigger_pos`
should cause indention, considering that there might be other potential
triggers `other_trigger_tokens` that are going to cause indention.
Indention is needed if the two conditions apply:

## Usage

``` r
needs_indention_one(pd, potential_trigger_pos, other_trigger_tokens)
```

## Arguments

- pd:

  A parse table.

- potential_trigger_pos:

  the index of the token in the parse table for which it should be
  checked whether it should trigger indention.

- other_trigger_tokens:

  Other tokens that are going to cause indention if on the same line as
  the token corresponding to `potential_trigger` and directly followed
  by a line break.

## Value

Returns `TRUE` if indention is needed, `FALSE` otherwise.

`TRUE` if indention is needed, `FALSE` otherwise.

## Details

- there is no multi-line token between the trigger and the first line
  break.

- there is no other token between the potential trigger and the first
  line break that is going to cause indention. Note that such an other
  trigger only causes indention if there is a line break after that
  other triggering token, not otherwise. If it causes indention, it is
  said to be an active trigger, if it does not, it is called an inactive
  trigger. See 'Details' for an example where there is an other trigger
  token, but since the next token is on the same line as the other
  trigger, the trigger is passive.

## Examples

``` r
style_text(c(
  "call(named = c,",
  "named = b)"
), strict = FALSE)
#> call(named = c,
#>   named = b)
```
