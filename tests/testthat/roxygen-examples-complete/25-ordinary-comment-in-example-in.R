#' Example
# Random comment
#' Roxygen
#' @examples
#' 1 + 1
NULL


#' Example
# Random comment
#' Roxygen
#' @examplesIf
#' 1 + 1
NULL

#' Example
# Random comment
#' Roxygen
#' @examples
#' 1 + 1
# comment
# more
NULL

#' Example
# Random comment
#' Roxygen
#' @examples
# There
#' 1 + 1
# comment
# more
NULL

#' Example
#' Random comment
#' Roxygen
#' @examples
# There
#' 1 + 1
# comment
# more
NULL

#' Example
# Random comment
#' Roxygen
#' @examples
# There
#' \dontrun{
#' 1 + 1
#' }
# comment
# more
NULL

#' Example
# Random comment
#' Roxygen
#' @examples
# There
#' \dontrun{
#' 1 + 1
#' } # comment
# more
NULL

#' Example
# Random comment
#' Roxygen
#' @examples
# 'There
#' \dontrun{
#' 1 + 1
#' }
# comment
# more
NULL

#' Example
# Random comment
#' Roxygen
#' @examples
# There
#' \dontrun{
# comment
#' 1 + 1
#' }
# more
NULL

#' Example
# Random comment
#' Roxygen
#' @examples
# There
#' \dontrun{
#' call(
# comment
#' 1 + 1
#' )
#' }
# more
NULL

# nolint start
#' @examplesIf TRUE
# nolint end
#' df %>% func()
func <- function() NULL


#' Hi
# Comment
#' @examples
#' 1 + 1
# this
# this
#this
# thi3
#' c()
NULL

#' Hi
# Comment
#' @examples
#' 1 + 1
# this
# this 
#this
# thi3
#' c()
NULL
