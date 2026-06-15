# Find the tokens to update when applying a reference indention

Given a target token and a flattened parse table, the token for which
the spacing information needs to be updated are computed. Since
indention is already embedded in the column `lag_spaces`, only tokens at
the beginning of a line are of concern.

## Usage

``` r
find_tokens_to_update(flattened_pd, target_token)
```

## Arguments

- flattened_pd:

  A flattened parse table.

- target_token:

  The index of the token from which the indention level should be
  applied to other tokens.

## See also

apply_ref_indention_one()

## Examples

``` r
style_text("function(a =
b,
dd
) {}", scope = "indention")
#> function(a =
#>     b,
#>   dd
#> ) {}
style_text("function(a,
b,
dd
) {}", scope = "indention")
#> function(a,
#>   b,
#>   dd
#> ) {}
```
