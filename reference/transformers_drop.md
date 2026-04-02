# Remove transformers that are not needed

The goal is to speed up styling by removing all rules that are only
applicable in contexts that don't occur often, e.g. for most code, we
don't expect ";" to be in it, so we don't need to apply
`resolve_semicolon()` on every *nest*.

## Usage

``` r
transformers_drop(text, transformers)
```

## Arguments

- text:

  Text to parse. Can also be the column `text` of the output of
  [`compute_parse_data_nested()`](https://styler.r-lib.org/reference/compute_parse_data_nested.md),
  where each element is a token (instead of a line).

- transformers:

  the transformers.

## See also

specify_transformers_drop
