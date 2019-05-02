c(a %>% b())

c(a %>% b())

c(a + b %>% c())

c(
  a %>% b()
)

c(a %>% b())

c(
  a %>% b() # 33
)

c(
  a + b %>% c()
)

c(
  a + b %>%
    c()
)

c(a + b %>%
  c())

c(
  a + b %>% # 654
    c()
)

c( # rr
  a + b %>%
    c()
)

c(
  a +
    b %>% c()
)

c(a +
  b %>% c())

a %>%
  b()

a %>%
  b()


fun(
  x,
  a %>% b()
)

fun(x,
  gg = a %>% b(),
  tt %>% q()
)

fun(x, gg = a %>% b(), tt %>% q())

z <- a %>%
  b()

fun(
  s = g(x),
  gg = a(n == 2) %>% b(),
  tt %>% q(r = 3)
)

blew(x %>%
  c(), y = 2)

blew(y = 2, x %>%
  c())


{
  a %>%
    c() + 1
}
