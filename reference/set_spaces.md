# Helper for setting spaces

Helper for setting spaces

## Usage

``` r
set_spaces(spaces_after_prefix, force_one)
```

## Arguments

- spaces_after_prefix:

  An integer vector with the number of spaces after the prefix.

- force_one:

  Whether spaces_after_prefix should be set to one in all cases.

## Value

An integer vector of length spaces_after_prefix, which is either one (if
`force_one = TRUE`) or `space_after_prefix` with all values below one
set to one.

Numeric vector indicating the number of spaces.
