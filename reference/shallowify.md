# Shallowify the parse table

Cached expressions don't need to be transformed with `transformers` in
[`parse_transform_serialize_r_block()`](https://styler.r-lib.org/reference/parse_transform_serialize_r_block.md),
we simply return `text` for the top-level token.

## Usage

``` r
shallowify(pd)
```

## Details

Expressions that are cached are already styled correctly. We can make
the parse table shallow at these locations, fully relying on the `text`
column:

- remove all children, as they are not needed anymore.

- mark the expression as a terminal.

## Top-level comments

Note that we do not cache top-level comments. Because package code has a
lot of roxygen comments and each of them is a top-level expression,
checking is very expensive. More expensive than styling, because
comments are always terminals. This will also yield large speed
improvements in
[`compute_parse_data_nested()`](https://styler.r-lib.org/reference/compute_parse_data_nested.md)
because nesting is expensive and will not be done for cached
expressions.

## Implementation

Because the structure of the parse table is not always "top-level
expression first, then children", this function creates a temporary
parse table that has this property and then extract the ids and subset
the original parse table so it is shallow in the right places.
