while (x > 3) {
  return(FALSE)
}

for (i in 1:3) {
  print(i)
}

if (x) {
  call2(3)
}

for (i in 1:3) { #
  print(i)
}

for (i in
  1:3) { #
  print(i)
}

for (i in #
  1:3) { #
  print(i)
}

for ( #
  i in #
  1:3 #
) { #
  print(i)
}


while (x > 3) { #
  return(FALSE)
}

while (x > 3 #
) {
  return(FALSE)
}

while ( # test
  x > 3) { # another
  return(FALSE)
}

while (
  2 > # here
    3 #
) { #
  FALSE
}

while (
  2 > # here
    3 #
) {
  FALSE
}

while (
  2 > # here
    3
) { #
  FALSE
}

while ( #
  2 >
    3
) { #
  FALSE
}
