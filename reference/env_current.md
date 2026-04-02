# The elements that are added to this environment are:

The elements that are added to this environment are:

## Usage

``` r
env_current
```

## Format

An object of class `environment` of length 0.

## Details

- `parser_version`: Needed to dispatch between parser versions, see
  [`parser_version_set()`](https://styler.r-lib.org/reference/parser_version_set.md)
  for details.

- `stylerignore`: A data frame with parse data containing tokens that
  fall within a stylerignore sequence. This is used after serializing
  the flattened parse table to apply the initial formatting to these
  tokens. See
  [stylerignore](https://styler.r-lib.org/reference/stylerignore.md) for
  details.

- `any_stylerignore`: Whether there is any stylerignore marker. The idea
  is to check early in the runtime if this is the case and then if so,
  take as many short-cuts as possible. See
  [stylerignore](https://styler.r-lib.org/reference/stylerignore.md) for
  details.
