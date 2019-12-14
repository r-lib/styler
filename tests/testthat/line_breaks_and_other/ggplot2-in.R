# don't remove line break
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  geom_point()


# add when unmasked
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) + geom_point()


# add when masked
ggplot2::ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) + geom_point()

# add when masked
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) + ggplot2::geom_point()

# add when comment
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) + # comment
  ggplot2::geom_point() + g()
