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
) {
  y
}


a <- function(x,
              y) {
  x - 1
}

a <- function(x,
              #
              y) {
  x - 1
}

a <- function(x,

              y) {
  x - 1
}


a <- function(
              x,
              y) {
  x - 1
}

a <- function( #
  x,
  y) {
  x - 1
}


# nested multi-line header
list(
  a = function(
    x,
    y) {
    x - 1
  }
)


# ambiguous case: closing parenthesis starts on new line (weirdly indented) -> open indent
a <- function(
              x,
              y
               ) {
  x - 1
}


# ambiguous case: closing parenthesis starts on new line -> open indent
a <- function(
              x,
              y
) {
  x - 1
}
