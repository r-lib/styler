# Communicate a warning if necessary

If round trip verification was not possible, issue a warning to review
the changes carefully.

## Usage

``` r
communicate_warning(changed, transformers)
```

## Arguments

- changed:

  Boolean with indicating for each file whether or not it has been
  changed.

- transformers:

  The list of transformer functions used for styling. Needed for reverse
  engineering the scope.
