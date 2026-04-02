# Visit'em all

Apply a list of functions to each level in a nested parse table.
`pre_visit()` applies `funs` before it proceeds to the children, (that
is, starts from the outermost level of nesting progressing to the
innermost level), `post_visit()` proceeds to its children before
applying the functions (meaning it first applies the functions to the
innermost level of nesting first and then going outwards).

## Usage

``` r
pre_visit(pd_nested, funs)

pre_visit_one(pd_nested, fun)

post_visit(pd_nested, funs)

post_visit_one(pd_nested, fun)
```

## Arguments

- pd_nested:

  A nested parse table.

- funs:

  A list of transformer functions.

## See also

Other visitors:
[`visit_one()`](https://styler.r-lib.org/reference/visit_one.md)
