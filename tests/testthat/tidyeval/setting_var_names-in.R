mtcars %>%
  group_by(am) %>%
  summarise(
    !!mean_nm:=mean(cyl),
    !!count_nm    :=n()
  )
