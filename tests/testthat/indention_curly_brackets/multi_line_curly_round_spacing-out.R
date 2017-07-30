b <- function(x) {
  x <- c(1,
         2 + 3,
         sin(pi)) # FIXME add tidyverse-comliant rule to break after '('

  if (x > 10) {
    return("done")
  }
}
