# Is the function declaration single indented?

Assumes you already checked if it's a function with
`is_function_declaration`. It is single indented if the first token
after the first line break that is a `"SYMBOL_FORMALS"`.

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
