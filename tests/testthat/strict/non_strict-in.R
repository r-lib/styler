test <- function() {
  "Double quotes remain as they are"
  'Single quotes are converted to double quotes'
  'even if the string contains an escaped \' single quote'
  'but not if it contains a "double quote'

  "multi-line quotes
  remain multi-line
  "

  'That also holds true
  if
  single quotes are used
  .'

  'strings with embeded\nline breaks are unfortunately split'

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
a   ~   b  : c

~   a
~gg
b~   k
call(1,~ qq)
