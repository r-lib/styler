# Set indention of tokens that match regex

Force the level of indention of tokens whose text matches a regular
expression pattern to be a certain amount of spaces. The rule is only
active for the first tokens on a line.

## Usage

``` r
set_regex_indention(
  flattened_pd,
  pattern,
  target_indention = 0L,
  comments_only = TRUE
)
```

## Arguments

- flattened_pd:

  A flattened parse table.

- pattern:

  A character with regular expressions to match against the token in
  `flattened_pd`.

- target_indention:

  The desired level of indention of the tokens that match `pattern`.

- comments_only:

  Boolean indicating whether only comments should be checked or all
  tokens.

## Value

A flattened parse table with indention set to `target_indention` for the
tokens that match `regex.`
