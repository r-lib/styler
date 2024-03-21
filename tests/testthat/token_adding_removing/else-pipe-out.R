mtcars %>%
  mutate(
    x = 1
  ) %>%
  if (FALSE) {
    mutate(., country = 2)
  } else .

# adding braces around . in else changes evaluation
