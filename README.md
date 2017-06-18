
<!-- README.md is generated from README.Rmd. Please edit that file -->
styler
======

[![Build Status](https://travis-ci.org/krlmlr/styler.svg?branch=master)](https://travis-ci.org/krlmlr/styler) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/krlmlr/styler?branch=master&svg=true)](https://ci.appveyor.com/project/lorenzwalthert/styler) [![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

The goal of styler is to provide non-invasive pretty-printing of R source code while adhering to the [tidyverse](https://github.com/tidyverse/style) formatting rules. Support for custom style guides is planned.

You can style a simple character vector of code with `style_text()`, a directory with `style_src()`or a whole R package with `style_pkg()`.

``` r
ugly_code <- "a<-function( x){1+1}           "
style_text(ugly_code)
#> [1] "a <- function(x) {1 + 1}"
```

You can find more information on the wiki of [Google Summer of Code 2017](https://github.com/rstats-gsoc/gsoc2017/wiki/Noninvasive-source-code-formatting) or check out the [pkgdown](https://krlmlr.github.io/styler/) page.

In the following, we present a dirty file and how it is styled with styler. The file can either be tidied, which is a less strict form of styling with an emphasis on preserving the programmer's intentions - or cleaned, which means to strictly apply the style guide at hand.

Dirty source file
-----------------

``` r
print_code <- function(x) {
  knitr::asis_output(
    paste(
      c("```r", "", "", "# ------", x, "# ------", "```"),
      collapse = "\n"))
}

dirty <- readLines(here::here("tests/testthat/example/in.R"))

print_code(dirty)


# ------
test <- function() {
  "Double quotes remain as they are"
  'Single quotes are converted to double quotes'
  'even if the string contains an escaped \' single quote'
  'but not if it contains a "double quote'

  # Comments are always preserved

  function_calls(get_spaces=around_equal)

  no_space( after_opening( ), paren( (1 + 2)))
  no_space (before_opening (), paren ( (1 + 2)))
  no_space(before(closing ), paren((1 + 2) ) )
  multi(
    line,
    call
  )
  multi_line_empty_call(
  )

  one_space(after,comma("in","function",  args))

  {
    braced
    expression
  }

  braced("unnamed", {
    "function"
    call
  })

  braced(named = {
    "function"
    call
  })

  braced("unnamed reduces space",    {
  })

  braced("unnamed adds space space",{
  })

  braced(named_reduces_space =    {
  })

  braced(named_adds_space =    {
  })

  braced(  {
    empty_removes_space
  })

  a%/%b
  a%%b
  a&&b
  a||b
  a==b
  a!=b
  a<=b
  a>=b
  a<-b
  a->b
  a=b
  a<b
  a>b
  a*b
  a/b
  a^b
  a&b
  a|b
  a:=b

  a+b
  a-b
  a++b
  a+-b
  a++b
  a-+b
  a--b
  a+--b
  a--+b
  call( + a)
  call( - a)
  call(5, + a)
  call(5, - a)

  # Only with conservative settings:
  call(
    preserves, distance,
    after,     commas,
    given_has,one
  )

  if(TRUE){
    FALSE
  }

  if(TRUE){
    FALSE
  }else{
    TRUE
  }

  while(TRUE){
    FALSE
  }

  single_line ( "function" ,call )

  multiline (
  "function", call )

  nested ( function_call ( "in" ,one ,line ) )

  nested ( function_call (
  "in",
      multiple,lines ) )

  nested(
  function_call ( with ),
      many
  ,     first_level_args  )

  nested(
    function_call ( with ),  # a comment and
    many #more
    ,     first_level_args  )

  difficult(nested(
    "function", call
  ),
    with, more, args
  )
}


# formula
lm(a~b+c,data=NA)
lm(a~.-1,data=NA)
a~b:c
a~b :c
# ------
```

Tidied file
-----------

``` r
tidy <- style_text(dirty, get_transformers(strict = FALSE))

print_code(tidy)


# ------
test <- function() {
  "Double quotes remain as they are"
  "Single quotes are converted to double quotes"
  "even if the string contains an escaped ' single quote"
  'but not if it contains a "double quote'

  # Comments are always preserved

  function_calls(get_spaces = around_equal)

  no_space(after_opening(), paren((1 + 2)))
  no_space (before_opening (), paren ((1 + 2)))
  no_space(before(closing), paren((1 + 2)))
  multi(
    line,
    call
  )
  multi_line_empty_call(
  )

  one_space(after, comma("in", "function",  args))

  {
    braced
    expression
  }

  braced("unnamed", {
    "function"
    call
  })

  braced(named = {
    "function"
    call
  })

  braced("unnamed reduces space",    {
  })

  braced("unnamed adds space space", {
  })

  braced(named_reduces_space =    {
  })

  braced(named_adds_space =    {
  })

  braced({
    empty_removes_space
  })

  a %/% b
  a %% b
  a && b
  a || b
  a == b
  a != b
  a <= b
  a >= b
  a <- b
  a -> b
  a = b
  a < b
  a > b
  a * b
  a / b
  a ^ b
  a & b
  a | b
  a := b

  a + b
  a - b
  a + +b
  a + -b
  a + +b
  a - +b
  a - -b
  a + --b
  a - -+b
  call(+a)
  call(-a)
  call(5, +a)
  call(5, -a)

  # Only with conservative settings:
  call(
    preserves, distance,
    after,     commas,
    given_has, one
  )

  if (TRUE) {
    FALSE
  }

  if (TRUE) {
    FALSE
  } else {
    TRUE
  }

  while (TRUE) {
    FALSE
  }

  single_line ("function" , call)

  multiline (
  "function", call)

  nested (function_call ("in" , one , line))

  nested (function_call (
  "in",
      multiple, lines))

  nested(
  function_call (with),
      many
  ,     first_level_args)

  nested(
    function_call (with),  # a comment and
    many #more
    ,     first_level_args)

  difficult(nested(
    "function", call
  ),
    with, more, args
  )
}


# formula
lm(a~b + c, data = NA)
lm(a~. - 1, data = NA)
a~b:c
a~b :c
# ------
```

Cleaned file
------------

``` r
clean <- styler::style_text(dirty)

print_code(clean)


# ------
test <- function() {
  "Double quotes remain as they are"
  "Single quotes are converted to double quotes"
  "even if the string contains an escaped ' single quote"
  'but not if it contains a "double quote'

  # Comments are always preserved

  function_calls(get_spaces = around_equal)

  no_space(after_opening(), paren((1 + 2)))
  no_space(before_opening(), paren((1 + 2)))
  no_space(before(closing), paren((1 + 2)))
  multi(
    line,
    call
  )
  multi_line_empty_call(
  )

  one_space(after, comma("in", "function", args))

  {
    braced
    expression
  }

  braced("unnamed", {
    "function"
    call
  })

  braced(named = {
    "function"
    call
  })

  braced("unnamed reduces space", {
  })

  braced("unnamed adds space space", {
  })

  braced(named_reduces_space = {
  })

  braced(named_adds_space = {
  })

  braced({
    empty_removes_space
  })

  a %/% b
  a %% b
  a && b
  a || b
  a == b
  a != b
  a <= b
  a >= b
  a <- b
  a -> b
  a = b
  a < b
  a > b
  a * b
  a / b
  a ^ b
  a & b
  a | b
  a := b

  a + b
  a - b
  a + +b
  a + -b
  a + +b
  a - +b
  a - -b
  a + --b
  a - -+b
  call(+a)
  call(-a)
  call(5, +a)
  call(5, -a)

  # Only with conservative settings:
  call(
    preserves, distance,
    after, commas,
    given_has, one
  )

  if (TRUE) {
    FALSE
  }

  if (TRUE) {
    FALSE
  } else {
    TRUE
  }

  while (TRUE) {
    FALSE
  }

  single_line("function" , call)

  multiline(
  "function", call)

  nested(function_call("in" , one , line))

  nested(function_call(
  "in",
      multiple, lines))

  nested(
  function_call(with),
      many
  , first_level_args)

  nested(
    function_call(with), # a comment and
    many #more
    , first_level_args)

  difficult(nested(
    "function", call
  ),
    with, more, args
  )
}


# formula
lm(a~b + c, data = NA)
lm(a~. - 1, data = NA)
a~b:c
a~b :c
# ------
```
