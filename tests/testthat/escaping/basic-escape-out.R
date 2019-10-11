#' things
#'
#' @examples
#' call("\\.")
NULL


#' things
#'
#' @examples
#' call("\n")
NULL

#' things
#'
#' @examples
#' call("\n")
#' ano("\\.", further = X)
NULL


#' things
#'
#' @examples
#' call("\n") # fixed: when single quotes are used, the newline is evaluated
#' ano("\\.", further = X)
NULL

'single quotes with
embedded and \n not embedded line breaks will not be replaced
with \\n double quotes. Too hard sorry.' # FIXME


x <- "	2" # there is a tab emebbed (created with writeLines("x <- '\t2'"))

x <- "\001"
"\001"

# FIXME: "\01" gives an error when not in a comment
# '\01'
