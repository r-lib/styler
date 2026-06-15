# Initialize default style guide attributes

This function initializes and removes various variables from the parse
table.

## Usage

``` r
default_style_guide_attributes(pd_flat)
```

## Arguments

- pd_flat:

  A parse table.

## Examples

``` r
withr::with_options(
  list(styler.cache_name = NULL), # temporarily deactivate cache
  {
    string_to_format <- "call( 3)"
    pd <- compute_parse_data_nested(string_to_format)
    styler:::pre_visit_one(pd, default_style_guide_attributes)
  }
)
#>   pos_id token terminal     text short token_before token_after stylerignore
#> 1      1  expr    FALSE call( 3) call(         <NA>        <NA>        FALSE
#>   block is_cached internal
#> 1     1     FALSE     TRUE
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    child
#> 1 3, 4, 6, 7, expr, '(', expr, ')', FALSE, TRUE, FALSE, TRUE, call, (, 3, ), call, (, 3, ), NA, SYMBOL_FUNCTION_CALL, NA, NUM_CONST, NA, NUM_CONST, NA, , FALSE, FALSE, FALSE, FALSE, NA, NA, NA, NA, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 2, SYMBOL_FUNCTION_CALL, TRUE, call, call, , '(', FALSE, NA, FALSE, FALSE, 0, 0, 0, 0, NA, 0, 5, NUM_CONST, TRUE, 3, 3, '(', ')', FALSE, NA, FALSE, FALSE, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, NA, 0, NA, 0, NA, NA, NA, NA, 0, 0, 0, 0
#>   newlines lag_newlines spaces multi_line indention_ref_pos_id indent
#> 1        0            0      0         NA                   NA      0
```
