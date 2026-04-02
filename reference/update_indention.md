# Update indention information of parse data

Update indention information of parse data

## Usage

``` r
indent_without_paren_for_while_fun(pd, indent_by)

indent_without_paren_if_else(pd, indent_by)

indent_braces(pd, indent_by)

indent_op(
  pd,
  indent_by,
  token = c(math_token, logical_token, special_token, "PIPE", "LEFT_ASSIGN", "EQ_ASSIGN",
    "'$'", "'~'")
)

indent_eq_sub(pd, indent_by, token = c("EQ_SUB", "EQ_FORMALS"))

indent_without_paren(pd, indent_by = 2L)
```

## Arguments

- pd:

  A nested or flat parse table that is already enhanced with line break
  and space information via
  [`default_style_guide_attributes()`](https://styler.r-lib.org/reference/default_style_guide_attributes.md).

- indent_by:

  How many spaces should be added after the token of interest.

- token:

  The token the indention should be based on.

## Functions

- `indent_without_paren_for_while_fun()`: Is used to indent for and
  statements and function definitions without parenthesis.

- `indent_without_paren_if_else()`: Is used to indent if and if-else
  statements.

- `indent_braces()`: Inserts indention based on round, square and curly
  brackets.

- `indent_op()`: Indents *all* tokens after `token` - including the last
  token.

- `indent_eq_sub()`: Updates indention for token EQ_SUB. Only differs
  from `indent_op()` in the sense that not all subsequent tokens in the
  parse table are necessarily indented, as `EQ_SUB` and `EQ_FORMALS` can
  occur multiple times in a parse table. occurs is not indented
  (see[`compute_indent_indices()`](https://styler.r-lib.org/reference/compute_indent_indices.md))

- `indent_without_paren()`: Is used to indent for / while / if / if-else
  statements that do not have curly parenthesis.
