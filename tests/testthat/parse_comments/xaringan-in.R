foo(
  data = mtcars,
  x = cyl,
  y = wt #<<
)


library(ggplot2)

ggplot(aes(x, y), data) +
  geom_point() + #<<
  scale_x_continuous() #<<
