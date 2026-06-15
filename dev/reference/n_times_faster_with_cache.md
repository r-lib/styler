# Times two function calls with temporarily enabled cache

This can be helpful for benchmarking.

## Usage

``` r
n_times_faster_with_cache(
  x1,
  x2 = x1,
  ...,
  fun = styler::style_text,
  n = 3L,
  clear = "always"
)
```

## Arguments

- ...:

  Arguments passed to `fun`.

- fun:

  The function that should be timed.

- n:

  The number of times the experiment should be repeated.

## Value

A scalar indicating the relative difference of the second compared to
the first run.
