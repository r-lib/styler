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
  emulated <- emulate_rd(roxygen)
  connection <- textConnection(emulated$text)
  had_warning <- FALSE
  parsed <- withCallingHandlers(
    {
      parsed <- as.character(tools::parse_Rd(connection, fragment = TRUE), deparse = FALSE)
      if (had_warning) {
        roxygen_remove_extra_brace(parsed)
      } else {
        parsed
      }
    },
    warning = function(w) {
      had_warning <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  close(connection)
  list(text = parsed, example_type = emulated$example_type)
}

#' Fix [tools::parse_Rd()] output
#'
#' Since [tools::parse_Rd()] treats braces in quotes as literal braces when
#' determining brace symmetry, a brace might be added in error to the parsed
#' data (at the end). We'll remove one at the time, check if output is parsable
#' until no braces are left. If we end up with no braces left, we signal a
#' parsing error, otherwise, we return the initial (not parsable input due to
#' *dont* sequence) with the trailing braces removed.
#' @examples
#' styler:::parse_roxygen(
#'   c(
#'     "#' @examples",
#'     "#' x <- '{'",
#'     "#' \\dontrun{",
#'     "#' fu(x = 3)",
#'     "#' }"
#'   )
#' )
#' styler:::parse_roxygen(
#'   c(
#'     "#' @examples",
#'     "#' x <- '{'",
#'     "#' \\dontrun{",
#'     "#' c('{', \"'{{{\" ,\"[\")",
#'     "#' }"
#'   )
#' )
#' @keywords internal
roxygen_remove_extra_brace <- function(parsed) {
  parsed <- rlang::with_handlers(
    {
      parse(text = paste0(gsub("^\\\\[[:alpha:]]*", "", parsed), collapse = ""))
      parsed
    },
    error = function(e) {
      # might have extra braces that are not needed: try to remove them

      # if fails, you need initial input for best error message
      parsed_ <- gsub("^\\\\[[:alpha:]]+", "", parsed)
      worth_trying_to_remove_brace <- any(parsed == "}")
      if (worth_trying_to_remove_brace) {
        # try to remove one and see if you can parse. If not, another one, until
        # you don't have any brace left.

        while (worth_trying_to_remove_brace) {
          # remove brace
          brace <- which(parsed == "}")
          if (length(brace) > 0) {
            parsed <- parsed[-last(brace)]
          }
          linebreak <- which(parsed == "\n")
          if (length(linebreak) > 0) {
            parsed <- parsed[-last(linebreak)]
          }
          # try if can be parsed (need remve dontrun)
          worth_trying_to_remove_brace <- rlang::with_handlers(
            {
              parse(text = gsub("^\\\\[[:alpha:]]+", "", parsed)) # this will error informatively
              FALSE # if parsing succeeds, we can stop tryint to remove brace and move on with parsed
            },
            error = function(...) {
              # continue if braces are left, otherwise give up
              if (any(last(parsed) %in% c("}", "\n"))) {
                TRUE
              } else {
                # this will error informatively. If not, outer loop will fail informatively
                parse(text = gsub("^\\\\[[:alpha:]]+", "", parsed_))
                FALSE
              }
            }
          )
        }
      } else {
        parse(text = gsub("^\\\\[[:alpha:]]*", "", parsed_)) # this will error informatively
      }
      parsed
    }
  )
}

#' Convert roxygen comments to Rd code
#'
#' We leverage roxygen2 workhorse function [roxygen2::roc_proc_text()] if
#' our input contains character that have to be escaped. Since this is an
#' expensive operation, we opt out of it and perform a simple
#' `remove_roxygen_mask()` when there are no characters to escape.
#' @keywords internal
emulate_rd <- function(roxygen) {
  example_type <- gsub("^#'(\\s|\t)*@examples(If)?(\\s|\t)*(.*)", "examples\\2", roxygen[1])
  if (needs_rd_emulation(roxygen)) {
    roxygen <- c(
      "#' Example",
      gsub("^#'(\\s|\t)*@examples(If)?(\\s|\t)*(.*)", "#' @examples \\4", roxygen),
      "x <- 1"
    )
    text <- roxygen2::roc_proc_text(
      roxygen2::rd_roclet(),
      paste(roxygen, collapse = "\n")
    )[[1]]$get_section("examples") %>%
      as.character() %>%
      .[-1]
    text <- c(
      if (grepl("^#'(\\s|\t)*@examples(\\s|\t)*$", roxygen[2])) "",
      text
    )
  } else {
    text <- remove_roxygen_mask(roxygen)
  }
  list(
    text = text,
    example_type = example_type
  )
}

#' Check if rd emulation is required with [roxygen2::roc_proc_text()]
#' @keywords internal
needs_rd_emulation <- function(roxygen) {
  # escape characters \ and % count, but not macros like \dontrun
  any(grepl("\\\\|%", gsub("^#'\\s*\\\\[[:alpha:]]*", "", roxygen)))
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
