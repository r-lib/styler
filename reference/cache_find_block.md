# Find the groups of expressions that should be processed together

Find the groups of expressions that should be processed together

## Usage

``` r
cache_find_block(pd)
```

## Arguments

- pd:

  A top-level nest.

## Details

We want blocks to be formed according to these rules:

- Blocks should contain either cached or uncached expressions only. If a
  block contains cached expressions only, it does not have to be
  processed and can be returned immediately. If a block contains
  uncached expressions, it makes sense to put as many uncached
  expression in it, since processing one bigger block has less overhead
  than processing many smaller blocks.

- Multiple expressions can sit on one row, e.g. in-line comment and
  commands separated with ";". This creates a problem when processing
  each expression separately because when putting them together, we need
  complicated handling of line breaks between them, as it is not *a
  priori* clear that there is a line break separating them. To avoid
  this, we put top-level expressions that sit on the same line into one
  block, so the assumption that there is a line break between each block
  of expressions holds.

- All expressions in a stylerignore sequence must be in the same block.
  If that's not the case, the first expression in a block might not be a
  top-level terminal, but another top-level expression.
  [`apply_stylerignore()`](https://styler.r-lib.org/reference/apply_stylerignore.md)
  joins `env_current$stylerignore`, which contains only terminals, with
  the first expression in a stylerignore sequence, based on the first
  `pos_id` in that stylerignore sequence (`first_pos_id_in_segment`).
