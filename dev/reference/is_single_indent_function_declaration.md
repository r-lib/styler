# Is the function declaration single indented?

Assumes you already checked if it's a function with
`is_function_declaration`. "single indented" means the formals are on
the line after `function(` and indented one further level, and the
closing `)` is also on its own line. The alternative compliant style
within the style guide is "hanging indented" where formals share the
line with `function(`, any subsequent lines of formals are indented
relative to that `(`, and the closing `)` shares a line with the last
formal argument.

## Usage

``` r
is_single_indent_function_declaration(pd, indent_by = 2L)
```

## Arguments

- pd:

  A parse table.

- indent_by:

  How many spaces of indention should be inserted after operators such
  as '('.
