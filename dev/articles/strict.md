# The effect of \`strict = FALSE\`

This vignette shows how output from styler might differ when
`strict = FALSE`. For brevity, we don’t show the output of
`strict = TRUE`, but it should be pretty simple for the user to derive
it from the bullet point(s) or simply paste the code in the console to
see the output.

``` r

library(styler)
```

- multi-line function declarations without curly braces are tolerated.

``` styler
function()
  NULL
```

- Spaces before opening parenthesis, tilde as well as around comments
  and math token must be at least one, not exactly one.

``` styler
1  +    (1 + 3)
1 ~  more()   #   comment
```

- Line breaks between curly and round braces are not removed.

``` styler
test({
  1
}
)
```

- Multi-line calls don’t put the closing brace on a new line nor trigger
  a line break after the opening brace.

``` styler
call(
  this)
call(2,
  more
)
```

- No line break inserted after pipes nor ggplot2 or pipe expressions.

``` styler
ggplot2::ggplot(data, aes(x, y)) + geom_line() + scale_x_continuous()

this %>% is() %>% a() %>% long() %>% pipe()
```

- ifelse statements don’t get curly braces added when multi-line.

``` styler
if (TRUE) 3  else
  5
```
