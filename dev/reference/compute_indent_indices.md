# Compute the indices that need indention

Based on `token`, find the rows in `pd` that need to be indented.

## Usage

``` r
compute_indent_indices(pd, token_opening, token_closing = NULL)
```

## Arguments

- pd:

  A parse table.

- token_opening:

  A character vector with tokens that could induce indention for
  subsequent tokens.

- token_closing:

  A character vector with tokens that could terminate indention for
  previous tokens. If `NULL` (the default), indention should end with
  the last token in the parse table.

## Details

Two cases are fundamentally different:

- Indention based on operators (e.g '+'), where all subsequent tokens
  should be indented.

- Indention based on braces (e.g. '('), where just the tokens between
  the opening and the closing brace have to be indented.

To cover the second case, we need `token_closing` because it cannot be
taken for granted that `token_closing` is always the last token in `pd`.
For example in if-else expressions, this is not the case and indenting
everything between '(' and the penultimate token would result in the
wrong formatting.

## Handing of `[[`

Since text `[[` has token `"LBB"` and text `]]` is parsed as two
independent `]` (see 'Examples'), indention has to stop at the first
`]`.

## Examples

``` r
styler:::parse_text("a[1]")
#> a[1]
styler:::parse_text("a[[1\n]]")
#> a[[1]]
```
