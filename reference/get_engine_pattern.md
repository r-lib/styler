# What's the engine pattern for rmd code chunks?

The function returns the regular expression pattern that identifies all
r engines in Rmd chunks. Defaults to `[Rr]`. You probably only want to
change this if you create a knitr engine that processes R code but is
not the default engine `r`. The pattern must be followed by a space (in
the case the chunk is given a name), a comma (if no name is given but
further options are passed to the engine) or a closing curly brace (in
case no option and no name is given to the chunk).

## Usage

``` r
get_engine_pattern()
```
