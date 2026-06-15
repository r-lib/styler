# Check if if, for or while loop expression require a braces.

This is the case if they are multi-line and not yet wrapped into curly
braces.

## Usage

``` r
if_for_while_part_requires_braces(pd, key_token)
```

## Arguments

- pd:

  A parse table.

- key_token:

  The token that comes right before the token that contains the
  expression to be wrapped (ignoring comments). For if and while loops,
  this is the closing "')'", for a for-loop it's "forcond".
