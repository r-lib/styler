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
#' call('\n')
#' ano("\\.", further = X)
NULL

'single quotes with
embedded and \n not embedded line breaks'

x <- '	2' # there is a tab emebbed (created with writeLines("x <- '\t2'"))

x <- '\001'
'\x01'

# FIXME: "\01" gives an error when not in a comment
# FIXME: this too: '\01'
