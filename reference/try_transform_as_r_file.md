# Style a file as if it was an .R file

If not successful, the file is most likely not a .R file, so saving the
file and try styling again will work if the file is an .Rmd file.
Otherwise, we can throw an error that the file must be a .R or .Rmd
file.

## Usage

``` r
try_transform_as_r_file(context, transformer)
```

## Arguments

- context:

  The context from `styler:::get_rstudio_context()`.

- transformer:

  A transformer function most conveniently constructed with
  [`make_transformer()`](https://styler.r-lib.org/reference/make_transformer.md).
