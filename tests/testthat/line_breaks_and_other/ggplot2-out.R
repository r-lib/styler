# don't remove line break
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  geom_point()


# add when unmasked
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  geom_point()


# add when masked
ggplot2::ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  geom_point()

# add when masked
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  ggplot2::geom_point()

# add when comment
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) + # comment
  ggplot2::geom_point() +
  g()


# add when comment
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  ggplot2::geom_point() +
  g() # comment

# add when comment
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  ggplot2::geom_point() +
  g() # comment


# add when comment
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  ggplot2::geom_point() +
  g() +
  geom_oint() # comment

# when subsetted involved
x[1] + c()

g() + x[1]

g()[2] + x[1]
