a <- function(x, #
              y) {
  x - 1
}


a <- function(x, #
              y) #
{
  x
}

a <- function(x, #
              y #
            ) { # FIXME: Move to the same indention level as a
  y
}
