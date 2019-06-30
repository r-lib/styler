
##  ............................................................................
##  line breaks                                                             ####
# not inserting line breaks
call({{ x }})

# removing line breaks
call({{
  x
}})

call({
  {x
}})

call({
  {x}}
)

call({
  {x}
  })

call(
  {
  {x
  }
    }
  )

##  ............................................................................
##  spaces                                                                  ####

# not inserting spaces between braces
call({{ x }})

# removing spaces between braces
call({ { x }})
call({ { x }} )
call( { { x }})
call( { { x } })

# inserting spaces within {{
call({{x }})
call({{x}})
call({{ x}})

# not removing spaces within {{
call({{ x }})


# combine spaces and line breaks
call({{ x}
  })

call({
  { x}})

# not applicable when only one curly brace
{
  y
}
{ 1 + 1}
{{1 + a} + 1} # not curly-culry!


##  ............................................................................
##  multiple                                                                ####
call({
  1
}, a + b, { 33 / f(c)})

call({{ x }}, {{ y}})
call({{ x }}, {{ y}
  })
call(
  {{ x }}, {{ y}})

call(
  {{ x }},
  {{ y}} := 3, f(bk)
)
