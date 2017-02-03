test <- function() {
  "Double quotes remain as they are"
  "Single quotes are converted to double quotes"
  'but not if the string contains a "double quote'
  'or an escaped \' single quote'

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
}
