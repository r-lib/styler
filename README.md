<!-- README.md is generated from README.Rmd. Please edit that file -->
styler
======

[![Build Status](https://travis-ci.org/krlmlr/styler.svg?branch=master)](https://travis-ci.org/krlmlr/styler)

The goal of styler is to provide non-invasive pretty-printing of R source code while adhering to general formatting conventions.

Dirty source file
-----------------

``` r
print_code <- function(x) {
  knitr::asis_output(
    paste(
      c("```r", "", "", "# ------", x, "# ------", "```"),
      collapse = "\n"))
}

dirty <- readLines("tests/testthat/example/in.R")

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
