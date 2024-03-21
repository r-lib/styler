while (TRUE) {
  3
}

# styler: off
while(TRUE)
  3

# styler: on
while (TRUE)
  # styler: off
  3

# styler: on

for (i # styler: off
  in 3)
  3

# styler: off
for (i
     in 3)
  3
# styler: on


# styler: off
for (i
     in 3)  {
  3}
# styler: on


for (i
  in 3) {
  3} # styler: off

for (i
     in 3)  {# styler: off
  3
}

for (i# styler: off
  in 3) {
  3
}


while (
  FALSE
) {
  # styler: off
  1
  # styler: on
}

while (
  FALSE # comment
) {
  # styler: off
  1
  # styler: on
}

while( # styler: off
  FALSE
) {
  1
}

while (
  # styler: off
  FALSE
) {

  1

}
# styler: on
