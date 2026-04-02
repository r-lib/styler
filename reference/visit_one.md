# Transform a flat parse table with a list of transformers

Uses [`Reduce()`](https://rdrr.io/r/base/funprog.html) to apply each
function of `funs` sequentially to `pd_flat`.

## Usage

``` r
visit_one(pd_flat, funs)
```

## Arguments

- pd_flat:

  A flat parse table.

- funs:

  A list of transformer functions.

## See also

Other visitors: [`visit`](https://styler.r-lib.org/reference/visit.md)
