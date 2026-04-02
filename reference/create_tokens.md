# Create a terminal token

Creates a terminal token represented as (a row of) a parse table.

## Usage

``` r
create_tokens(
  tokens,
  texts,
  lag_newlines = 0L,
  spaces = 0L,
  pos_ids,
  token_before = NA,
  token_after = NA,
  indention_ref_pos_ids = NA,
  indents,
  terminal = TRUE,
  child = NULL,
  stylerignore,
  block = NA,
  is_cached = FALSE
)
```

## Arguments

- tokens:

  Character vector with tokens to create.

- texts:

  Character vector with texts of the token to create.

- lag_newlines:

  Character vector with lag_newlines corresponding to the tokens.

- spaces:

  Character vector with spaces corresponding to the tokens.

- pos_ids:

  Character vector with positional id corresponding to the tokens.

- token_before:

  Character vector corresponding to the columns `token_before`.

- token_after:

  Character vector corresponding to the columns `token_after`.

- indention_ref_pos_ids:

  Character vector with indention ref ids corresponding to the tokens.

- indents:

  Vector with indents corresponding to the tokens.

- terminal:

  Boolean vector indicating whether a token is a terminal or not.

- child:

  The children of the tokens.

- stylerignore:

  Boolean to indicate if the line should be ignored by styler. Must take
  value from token before, can't have a default.

- block:

  The block (of caching) to which the token belongs. An integer.

- is_cached:

  Whether the token is cached already.

## See also

Other token creators:
[`create_pos_ids()`](https://styler.r-lib.org/reference/create_pos_ids.md),
[`validate_new_pos_ids()`](https://styler.r-lib.org/reference/validate_new_pos_ids.md)
