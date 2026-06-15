# Start comments with a space

Forces comments to start with a space, that is, after the regular
expression `#+['\\*]`, at least one space must follow if the comment is
*non-empty*, i.e there is not just spaces within the comment. Multiple
spaces may be legit for indention in some situations.

## Usage

``` r
start_comments_with_space(pd, force_one = FALSE)
```

## Arguments

- pd:

  A parse table.

- force_one:

  Whether or not to force one space or allow multiple spaces.

## Exceptions

Spaces won't be added to comments when they are:

- shebangs

- code chunk headers

- xaringan markers
