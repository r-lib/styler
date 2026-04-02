# What is a parse table representing?

Check whether a parse table corresponds to a certain expression.

## Usage

``` r
is_curly_expr(pd)

is_for_expr(pd)

is_conditional_expr(pd)

is_while_expr(pd)

is_function_call(pd)

is_function_declaration(pd)

is_comment(pd)

is_tilde_expr(pd, tilde_pos = c(1L, 2L))

is_asymmetric_tilde_expr(pd)

is_symmetric_tilde_expr(pd)
```

## Arguments

- pd:

  A parse table.

- tilde_pos:

  Integer vector indicating row-indices that should be checked for
  tilde. See 'Details'.

## Details

A tilde is on the top row in the parse table if it is an asymmetric
tilde expression (like `~column`), in the second row if it is a
symmetric tilde expression (like `a~b`).

## Functions

- `is_curly_expr()`: Checks whether `pd` contains an expression wrapped
  in curly brackets.

- `is_for_expr()`: Checks whether `pd` contains a `for` loop.

- `is_conditional_expr()`: Checks whether `pd` contains is a conditional
  expression.

- `is_while_expr()`: Checks whether `pd` contains a `while` loop.

- `is_function_call()`: Checks whether `pd` is a function call.

- `is_function_declaration()`: Checks whether `pd` is a function
  declaration.

- `is_comment()`: Checks for every token whether or not it is a comment.

- `is_tilde_expr()`: Checks whether `pd` contains a tilde.

- `is_asymmetric_tilde_expr()`: If `pd` contains a tilde, checks whether
  it is asymmetrical.

- `is_symmetric_tilde_expr()`: If `pd` contains a tilde, checks whether
  it is symmetrical.

## See also

Other third-party style guide helpers:
[`next_non_comment()`](https://styler.r-lib.org/reference/next_non_comment.md),
[`scope_normalize()`](https://styler.r-lib.org/reference/scope_normalize.md)

## Examples

``` r
code <- "if (TRUE) { 1 }"
pd <- compute_parse_data_nested(code)
is_curly_expr(pd)
#> [1] FALSE
child_of_child <- pd$child[[1]]$child[[5]]
is_curly_expr(child_of_child)
#> [1] TRUE

code <- "for (i in 1:5) print(1:i)"
pd <- compute_parse_data_nested(code)
is_for_expr(pd)
#> [1] FALSE
is_for_expr(pd$child[[1]])
#> [1] TRUE

code <- "if (TRUE) x <- 1 else x <- 0"
pd <- compute_parse_data_nested(code)
is_conditional_expr(pd)
#> [1] FALSE
is_conditional_expr(pd$child[[1]])
#> [1] TRUE

code <- "x <- list(1:3)"
pd <- compute_parse_data_nested(code)
is_function_call(pd)
#> [1] FALSE
child_of_child <- pd$child[[1]]$child[[3]]
is_function_call(child_of_child)
#> [1] TRUE

code <- "foo <- function() NULL"
pd <- compute_parse_data_nested(code)
is_function_declaration(pd)
#> [1] FALSE
child_of_child <- pd$child[[1]]$child[[3]]
is_function_declaration(child_of_child)
#> [1] TRUE

code <- "x <- 1 # TODO: check value"
pd <- compute_parse_data_nested(code)
is_comment(pd)
#> [1] FALSE  TRUE

code <- "lm(wt ~ mpg, mtcars)"
pd <- compute_parse_data_nested(code)
is_tilde_expr(pd$child[[1]]$child[[3]])
#> [1] TRUE
is_symmetric_tilde_expr(pd$child[[1]]$child[[3]])
#> [1] TRUE
is_asymmetric_tilde_expr(pd$child[[1]]$child[[3]])
#> [1] FALSE
```
