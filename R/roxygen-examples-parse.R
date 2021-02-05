#' Parse roxygen comments into text
#'
#' Used to parse roxygen code examples. Removes line break before
#' `\\dontrun{...}` and friends because it does not occur for segments other
#' than `\\dont{...}` and friends.
#' @param roxygen Roxygen comments.
#' @examples
#' styler:::parse_roxygen(c(
#'   "#' @examples",
#'   "#' 1+  1"
#' ))
#' styler:::parse_roxygen(c(
#'   "#' @examples 33",
#'   "#'1+  1"
#' ))
#' @keywords internal
parse_roxygen <- function(roxygen) {
  roxygen <- c(
    "#' Example", "#' @examples",
    gsub("^#'\\s*@examples\\s*(.*)", "#' \\1", roxygen, perl = TRUE),
    "x <- 1"
  )

  connection <- roxygen2::roc_proc_text(
    roxygen2::rd_roclet(),
    paste(roxygen, collapse = "\n")
  )[[1]]$get_section("examples") %>%
    as.character() %>%
    .[-1] %>%
    textConnection()
  suppressWarnings(
    parsed <- tools::parse_Rd(connection, fragment = TRUE) %>%
      as.character(deparse = FALSE)
  )
  parsed <- rlang::with_handlers(
    {
      # dont match word boundary, check this matches all keywords
      parse_safely(paste0(gsub("^\\\\[[:alpha:]]*\\b", "", parsed, perl = TRUE), collapse = ""))
      parsed
    },
    error = function(e) {
      parsed_ <- gsub("\\\\.*\\w", "", parsed)
      if (any(parsed == "}")) {
        parsed <- parsed[-last(which(parsed == "}"))]
        parsed[-last(which(parsed == "\n"))]
      } else {
        parse_safely(gsub("\\\\.*\\w", "", parsed_)) # this will error informatively
      }
    }
  )
  parsed <- parsed[parsed != ""]
  is_line_break <- parsed[1] == "\n"
  close(connection)
  c(parsed[1][!is_line_break], parsed[-1])
}

#' Changing the line definition
#'
#' Input: New line denoted with `\\n`. Lines can span across elements.
#' Output: Each element in the vector is one line.
#'
#' @param raw Raw code to post-process.
#' @keywords internal
post_parse_roxygen <- function(raw) {
  raw %>%
    paste0(collapse = "") %>%
    convert_newlines_to_linebreaks()
}
