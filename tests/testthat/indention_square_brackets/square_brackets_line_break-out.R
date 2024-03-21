ranges[
  tag == "non_literal" & str_detect(text, ";"),
  text := str_replace_all(text, ";", "\n")
]

fak[a, b]

fac[
  a,
  b
]

fac[
  a,
  b
]

fac[
  , `:`(a = b)
]

fac[
  , `:`(a = b)
]

fac[, `:`(a = c)]

x[a == 3 |
  b == v, ]

x[a == 3 |
  b == v, ]

x[a == 3 ||
  b == v, ]

x[a == 3 ||
  b == v, ]

x[a == 3 &&
  b == v, ]

x[a == 3 &
  b == v, ]

x[a == 3 &&
  b == v, ]

x[a == 3 &
  b == v, ]
