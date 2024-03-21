c(a %>% b)

c(a %>% b())

c(a + b %>% c)

c(
  a %>% b)

c(a %>% b()
)

c(a %>% b() # 33
)

c(
  a + b %>% c
  )

c(
  a + b %>%
    c)

c(a + b %>%
    c)

c(
  a + b %>% # 654
    c
)

c( # rr
  a + b %>%
    c
)

c(
  a +
    b %>% c
)

c(a +
    b %>% c
)

a %>% b(
)

a %>% b(
) %>% q

a %>%
  b()

a %>% b() %>% c

# short pipes < 2 can stay on one line
a %>% b()

fun(x,
  a %>% b)

fun(x,
    gg = a %>% b,
    tt %>% q)

fun(x, gg = a %>% b, tt %>% q)

z = a %>% b()

fun( s = g(x),
    gg = a(n == 2) %>% b,
    tt %>% q(r = 3))

# FIXME closing brace could go on ntext line. Alternative: remove lin breaks completely.
blew(x %>%

       c(), y = 2)

# FIXME closing brace could go on ntext line. Alternative: move c() up.
blew(y = 2, x %>%
       c())


{a %>% c +1}


b %>%
  f() %>% # never move comment to next line as it can be styler: off or nolint
  k() %>%
  x()


# line break before { inserted inside and outside function calls
c(
data %>%
  filter(bar) %>% {
    cor(.$col1, .$col2, use = "complete.obs")
  }
)

data %>%
  filter(bar) %>% {
    cor(.$col1, .$col2, use = "complete.obs")
  }

# line break before { kept inside and outside function calls
c(
  data %>%
    filter(bar) %>%
    {
      cor(.$col1, .$col2, use = "complete.obs")
    }
)

data %>%
  filter(bar) %>%
  {
    cor(.$col1, .$col2, use = "complete.obs")
  }

# redundant blank lines removed
c(
  data %>%
    filter(bar) %>%

    {
      cor(.$col1, .$col2, use = "complete.obs")
    }
)

data %>%
  filter(bar) %>%

  {
    cor(.$col1, .$col2, use = "complete.obs")
  }

# blank lines kept when around comment
c(
  data %>%
    filter(bar) %>%
    # comment

    {
      cor(.$col1, .$col2, use = "complete.obs")
    }
)

data %>%
  filter(bar) %>%
  # comment

  {
    cor(.$col1, .$col2, use = "complete.obs")
  }
