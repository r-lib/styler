ranges[
  tag == "non_literal" & str_detect(text, ";"),
  text := str_replace_all(text, ";", "\n")
]

fak[a, b]

fac[a, b]

fac[
  a,
  b
]

fac[,
  `:`(a = b)
]

fac[,
  `:`(a = b)
]

fac[, `:`(a = c)]

fac[,
  `:`(a = c)
]

x[
  a == 3 |
    b == v,
]

x[a == 3 | b == v, ]

x[
  a == 3 ||
    b == v,
]

x[a == 3 || b == v, ]

x[a == 3 && b == v, ]

x[a == 3 & b == v, ]

x[
  a == 3 &&
    b == v,
]

x[
  a == 3 &
    b == v,
]

x[
  # comments above
  a == 3 &
    b == v,
  # or below shouldn't be an issue
]

x[
  a,
  b
]

x[
  # this comment shouldn't be an issue
  1,
  c(
    1,
    2

    # neither should this one
  )
]
