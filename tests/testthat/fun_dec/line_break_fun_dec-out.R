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


a <- function(x,
              y) {
  x - 1
}

a <- function(
  #
  x,
  y
) {
  x - 1
}


# nested multi-line header
list(
  a = function(
    x,
    y
  ) {
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


# mixed structure A: first formal on same line, subsequent formal on new line (standard indent <= 4 spaces) -> open multi-line
f <- function(
  a =
    1,
  b = 2
) {}


# mixed structure B: first formal on same line, subsequent formal on new line (heavily indented > 4 spaces) AND ')' on same line -> preserve aligned
f <- function(a = 1,
              b = 2) {}


# mixed structure C: first formal on same line, subsequent formal on new line (heavily indented > 4 spaces) BUT ')' starts on new line -> open multi-line
f <- function(a =
                1,
              b = 2) {}
