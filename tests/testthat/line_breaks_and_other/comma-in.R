call(a,
     b
     , c)

call(a, b
     ,
     c)

call(a,)
call(a,
)

call(a
     ,)

mpg %>%
    summarise(avg_cty = mean(cty)
, avg_hwy = mean(hwy)
, n = n()
, n_class = n_distinct(class))
