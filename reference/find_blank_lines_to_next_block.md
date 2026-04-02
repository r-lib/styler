# Number of lines between cache blocks

This is relevant when putting expressions together into a block and
preserve blank lines between them. Note that because code does not need
to start on line 1, the first element of the output is the number of
lines until the first block.

## Usage

``` r
find_blank_lines_to_next_block(pd)
```

## Arguments

- pd:

  A top-level nest.
