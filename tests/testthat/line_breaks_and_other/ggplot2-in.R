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


# add when comment
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  ggplot2::geom_point() + g() # comment

# add when comment
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) + ggplot2::geom_point() + g() # comment


# add when comment
ggplot(data = mtcars, mapping = aes(x = mpg, y = vs)) +
  ggplot2::geom_point() + g()  + geom_oint() # comment

# when subsetted involved
x[1]+ c()

g() + x[1]

g()[2] + x[1]

# don't do anything on unary + and function call
+sin(x)

# within function call
qqjflk(
  log(y + 1) +
    # sqrt(x1) +
    sqrt(x2) +
    # sqrt(x3) +
    x4 +
    sqrt(x5)
)
