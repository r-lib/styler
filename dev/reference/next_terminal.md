# Tell me what the next terminal is

If the first is a terminal, return it. If not, go inside it and search
the next terminal

## Usage

``` r
next_terminal(
  pd,
  stack = FALSE,
  vars = c("pos_id", "token", "text"),
  tokens_exclude = NULL
)
```

## Arguments

- pd:

  A nest.

- stack:

  Whether or not to also return information on the tokens that are
  between `pd` and the first terminal, so the returned data frame can be
  understood as a transition path from `pd` to the next terminal,
  instead of the information at the terminal only. The order is
  inside-out, i.e. the first non-terminal on top, the terminal last.

- vars:

  The variables to return.

- tokens_exclude:

  A vector with tokens to exclude. This can be helpful if one wants to
  find the next token that is not a comment for example.

## Value

Returns a data frame (which is **not** a valid parse table for
`stack = TRUE`), with `vars` and another variable `position` that
denotes the index each element in the transition. This can be helpful in
conjunction with
[`purrr::pluck()`](https://purrr.tidyverse.org/reference/pluck.html) or
[`purrr::modify_in()`](https://purrr.tidyverse.org/reference/modify_in.html)
to reach the terminal in the nested structure.

## Examples

``` r
withr::with_options(
  list(styler.cache_name = NULL), # temporarily deactivate cache
  {
    pd <- compute_parse_data_nested("if (TRUE) f()")
    styler:::next_terminal(pd)
  }
)
#>   position pos_id token text
#> 1        1      2    IF   if
```
