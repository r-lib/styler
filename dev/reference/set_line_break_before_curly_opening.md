# Set line break before a curly brace

Rule:

- Principle: Function arguments that consist of a braced expression
  always need to start on a new line

- Exception: unless it's the last argument and all other arguments fit
  on the line of the function call

- Exception: or they are named

- Extension: Also, expressions following on braced expressions also
  cause a line trigger

## Usage

``` r
set_line_break_before_curly_opening(pd)
```

## Examples

``` r
if (FALSE) {
tryCatch(
  {
    f(8)
  },
  error = function(e) NULL
)
# last-argument case
testthat("braces braces are cool", {
  code(to = execute)
})
call2(
  x = 2, {
    code(to = execute)
  },
  c = {
    # this is the named case
    g(x = 7)
  }
)
tryGugus(
  {
    g5(k = na)
  },
  a + b # line break also here because
  # preceded by brace expression
)

# brace expressions go on new line if part of a pipe, in function call...
c(
  data %>%
    filter(bar) %>%
    {
      cor(.$col1, .$col2, use = "complete.obs")
    }
)
# ... or outside
data %>%
  filter(bar) %>%
  {
    cor(.$col1, .$col2, use = "complete.obs")
  }
}
```
