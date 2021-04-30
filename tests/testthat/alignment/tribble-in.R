tribble(
  ~x,       ~d,
  "axa'fa", 1:6,
  "b",      4:6
)

tribble(
  ~x,       ~d,
  "axa'fa", 1:6,
  "b",    4:6
)


tribble(
  ~x,       ~d,
  "axa'fa", 1:6,
"b",      4:6
)

tribble(
  ~x,       ~d,
"axa'fa",  1:6,
  "b",      4:6
)

# has EQ_SUB which don't match, not tribble-like
mlr3misc:::rowwise_table(
  x = 23, zy = 3,
  y = 1,  k = 1,
)
