# Apply transformers to a parse table

The column `multi_line` is updated (after the line break information is
modified) and the rest of the transformers are applied afterwards, The
former requires two pre visits and one post visit.

## Usage

``` r
apply_transformers(pd_nested, transformers)
```

## Arguments

- pd_nested:

  A nested parse table.

- transformers:

  A list of *named* transformer functions

## Details

The order of the transformations is:

- Initialization (must be first).

- Line breaks (must be before spacing due to indention).

- Update of newline and multi-line attributes (must not change
  afterwards, hence line breaks must be modified first).

- spacing rules (must be after line-breaks and updating newlines and
  multi-line).

- indention.

- token manipulation / replacement (is last since adding and removing
  tokens will invalidate columns token_after and token_before).

- Update indention reference (must be after line breaks).
