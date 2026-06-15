# Save parsing from text

Parses text safely, i.e. throws an informative error if EOL style does
not match LF or indicates the exact position where the parsing failed.
Note that we can only detect wrong EOL style if it occurs on the first
line already.

## Usage

``` r
parse_safely(text, ...)
```

## Arguments

- text:

  Text to parse.

- ...:

  Parameters passed to
  [`base::parse()`](https://rdrr.io/r/base/parse.html).

## Examples

``` r
try(styler:::parse_safely("a + 3 -4 -> x\r\n glück + 1"))
#> Error : ✖ The code to style seems to use Windows style line endings (CRLF).
#> ! styler currently only supports Unix style line endings (LF).
#> ℹ Please change the EOL character in your editor to Unix style and try again.
#> Caused by error in `parse()`:
#> ! <text>:1:14: unexpected invalid token
#> 1: a + 3 -4 -> x
#>                  ^
# This cannot be detected as a EOL style problem because the first
# line ends as expected with \n
try(styler:::parse_safely("a + 3 -4 -> x\nx + 2\r\n glück + 1"))
#> Error : ✖ Styling failed
#> Caused by error in `parse()`:
#> ! <text>:2:6: unexpected invalid token
#> 1: a + 3 -4 -> x
#> 2: x + 2
#>         ^

styler:::parse_safely("a + 3 -4 -> \n glück + 1")
#> expression(glück + 1 <- a + 3 - 4)
```
