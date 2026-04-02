# Styling around `\{\{`

With {rlang} version 0.4, a new syntactic sugar is introduced, the
curly-curly operator. It interprets this code in a special way:
`call(\{\{ x \}\})`. See this [blog
post](https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/) on the topic.
Here, the curly-curly sugar is understood as two opening curly braces,
followed by an expression followed by two closing curly braces, e.g.
`\{\{1\}\}`. `\{\{1\} + 1\}` does not contain the curly-curly syntactic
sugar according to the above definition. On the other hand
`\{\{ x + y \}\}` is recognized by styler as containing it (and is
parsable code) but will most likely give an error at runtime because the
way the syntactic sugar is defined in rlang is to use a single token
within curly-curly. In addition, because rlang parses `\{\{` in a
special way (just as `!!`), the expression `\{\{ x \}\}` will give a
runtime error when used outside of a context that is capable of handling
it, e.g. on the top-level (that is, not within function call like
`rlang_fun(\{\{ x \}\})`) or within a base R function such as
[`c()`](https://rdrr.io/r/base/c.html). However, these differences are
assumed to be irrelevant for styling curly-curly, as much as they were
for styling `!!`. curly-curly affects styling of line break and spaces,
namely:

## Usage

``` r
set_line_break_around_curly_curly(pd)

set_space_in_curly(pd)
```

## Arguments

- pd:

  A parse table.

## Details

- No line break after first or second `\{`, before third and fourth
  `\{`.

- No space after first and third `\{`, one space after second and before
  third `\}`.

- No line breaks within curly-curly, e.g. `\{\{ x \}\}` can only contain
  line breaks after the last brace or before the first brace. But these
  are not dependent on curly-curly specifically.

## See also

style_text_without_curly_curly
