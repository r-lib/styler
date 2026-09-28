# formals on their own line -> single indentation, by indent_by
a <- function(
    x,
    y
) {
    x - 1
}


# formals indented by more than 2 * indent_by -> hanging indentation
a <- function(x,
              y) {
    x - 1
}


# formals indented by 2 * indent_by -> single indentation
f <- function(
    a = 1,
    b = 2
) {}


# nested multi-line header -> single indentation
list(
    a = function(
        x,
        y
    ) {
        x - 1
    }
)
