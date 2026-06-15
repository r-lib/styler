# Read UTF-8

Reads an UTF-8 file, returning the content and whether or not the final
line was blank. This information is required higher up in the call stack
because we should write back if contents changed or if there is no blank
line at the EOF. A perfectly styled file with no EOF blank line will
gain such a line with this implementation.

## Usage

``` r
read_utf8(path)
```

## Arguments

- path:

  A path to a file to read.
