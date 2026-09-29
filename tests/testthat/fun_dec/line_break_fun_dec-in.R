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


# nested multi-line header -> single indentation
list(
  a = function(
    x,
    y) {
    x - 1
  }
)


# ambiguous case: closing parenthesis starts on new line -> single indentation
a <- function(
              x,
              y
               ) {
  x - 1
}


# ambiguous case: closing parenthesis starts on new line -> single indentation
a <- function(
              x,
              y
) {
  x - 1
}


# mixed structure A: subsequent formal on new line (<= 4 spaces) -> single indentation
f <- function(a =
  1,
  b = 2
) {}


# mixed structure B: heavily indented (> 4 spaces) AND ')' on same line -> hanging indentation
f <- function(a = 1,
              b = 2) {}


# mixed structure C: heavily indented (> 4 spaces) BUT ')' starts on new line -> single indentation
f <- function(a =
                1,
              b = 2
) {}
