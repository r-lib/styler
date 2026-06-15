# Get the value of an option

Basically a `getOptions()` that fails fast by default.

## Usage

``` r
option_read(x, default = NULL, error_if_not_found = TRUE)
```

## Arguments

- x:

  a character string holding an option name.

- default:

  if the specified option is not set in the options list, this value is
  returned. This facilitates retrieving an option and checking whether
  it is set and setting it separately if not.

- error_if_not_found:

  Whether or not an error should be returned if the option was not set.
