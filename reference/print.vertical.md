# Print styled code

Print styled code

## Usage

``` r
# S3 method for class 'vertical'
print(
  x,
  ...,
  colored = getOption("styler.colored_print.vertical"),
  style = prettycode::default_style()
)
```

## Arguments

- x:

  A character vector, one element corresponds to one line of code.

- ...:

  Not currently used.

- colored:

  Whether or not the output should be colored with
  [`prettycode::highlight()`](https://rdrr.io/pkg/prettycode/man/highlight.html).

- style:

  Passed to
  [`prettycode::highlight()`](https://rdrr.io/pkg/prettycode/man/highlight.html).
