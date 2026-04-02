# Identify comments that are shebangs

Shebangs should be preserved and no space should be inserted between `#`
and `!`. A comment is a shebang if it is the first top-level token
(identified with `pos_id`) and if it starts with `#!`.

## Usage

``` r
is_shebang(pd)
```

## Arguments

- pd:

  A parse table.

## Examples

``` r
style_text("#!/usr/bin/env Rscript")
#> #!/usr/bin/env Rscript
```
