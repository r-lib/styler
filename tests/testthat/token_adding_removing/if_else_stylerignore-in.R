a =1
b=3
k = 9
h <- function() 1
s <- h
{
  if (TRUE) # styler: off
    3
  else
    5 # styler: off
}


{
  if (TRUE) { # styler: off
    3
    a + b
  }else
    5 # styler: off

  c()
}

# styler: off
{
  if (TRUE)
    3
  else {
    h()
    5 }
}
# styler: on

{
  if (TRUE) {
    3 # styler: off
  }else {
    s()
    5 }
}

if (TRUE) # styler: off
  1 else
    3

if (FALSE) # styler: off
  1 + a * ( 31/2) else
    3^k


if (TRUE)
  1+1 else # styler: off
    3

if (TRUE)
  1 + 1 else a +4

# styler: off
{if (TRUE)
  3
else
  5
}
# styler: on
