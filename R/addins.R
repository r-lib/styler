#' Stylers for RStudio Addins
#'
#' Helper functions for styling via RStudio Addins.
#'
#' @section Auto-Save Option:
#' By default, both of the RStudio Addins will apply styling to the (selected)
#' file contents without saving changes. Automatic saving can be enabled by
#' setting the environment variable `save_after_styling` to `TRUE`.
#'
#' Consider setting this in your `.Rprofile` file if you want to persist
#' this setting across multiple sessions. Untitled files will always need to be
#' saved manually after styling.
#'
#' @name styler_addins
#' @family stylers
#' @seealso [Sys.setenv()]
NULL

#' @describeIn styler_addins Styles the active file with [tidyverse_style()] and
#'   `strict = TRUE`.
#' @importFrom rlang abort
#' @keywords internal
style_active_file <- function() {
  communicate_addins_style()
  context <- get_rstudio_context()
  transformer <- make_transformer(get_addins_style_fun()(),
    include_roxygen_examples = TRUE, warn_empty = is_plain_r_file(context$path)
  )

  if (is_rmd_file(context$path)) {
    out <- transform_mixed(context$contents, transformer, filetype = "Rmd")
  } else if (is_rnw_file(context$path)) {
    out <- transform_mixed(context$contents, transformer, filetype = "Rnw")
  } else if (is_plain_r_file(context$path) | is_unsaved_file(context$path)) {
    out <- try_transform_as_r_file(context, transformer)
  } else {
    abort("Can only style .R, .Rmd and .Rnw files.")
  }
  rstudioapi::modifyRange(
    c(1, 1, length(context$contents) + 1, 1),
    paste0(ensure_last_is_empty(out), collapse = "\n"),
    id = context$id
  )
  if (Sys.getenv("save_after_styling") == TRUE && context$path != "") {
    rstudioapi::documentSave(context$id)
  }
  rstudioapi::setCursorPosition(context$selection[[1]]$range)
}

#' Style a file as if it was an .R file
#'
#' If not successful, the file is most
#' likely not a .R file, so saving the file and try styling again will work if
#' the file is an .Rmd file. Otherwise, we can throw an error that the file must
#' be a .R or .Rmd file.
#' @param context The context from `styler:::get_rstudio_context()`.
#' @param transformer A transformer function most conveniently constructed with
#'   [make_transformer()].
#' @importFrom rlang with_handlers abort
#' @keywords internal
try_transform_as_r_file <- function(context, transformer) {
  with_handlers(
    transformer(context$contents),
    error = function(e) {
      preamble_for_unsaved <- paste(
        "Styling of unsaved files is only supported for R files with valid code.",
        "Please save the file (as .R or .Rmd) and make sure that the R code in it",
        "can be parsed. Then, try to style again."
      )

      if (context$path == "") {
        abort(paste0(preamble_for_unsaved, " The error was \n", e$message))
      } else {
        abort(e$message)
      }
    }
  )
}

#' @describeIn styler_addins Styles the highlighted selection in a `.R` or
#'   `.Rmd` file.
#' @importFrom rlang abort
#' @keywords internal
style_selection <- function() {
  communicate_addins_style()
  context <- get_rstudio_context()
  text <- context$selection[[1]]$text
  if (all(nchar(text) == 0)) abort("No code selected")
  out <- style_text(text, style = get_addins_style_fun())
  rstudioapi::modifyRange(
    context$selection[[1]]$range, paste0(out, collapse = "\n"),
    id = context$id
  )
  if (Sys.getenv("save_after_styling") == TRUE && context$path != "") {
    rstudioapi::documentSave(context$id)
  }
}

get_rstudio_context <- function() {
  rstudioapi::getActiveDocumentContext()
}

#' Ask the user to supply a style
#'
#' @importFrom rlang abort
#' @keywords internal
#' @importFrom rlang with_handlers abort
prompt_style <- function() {
  current_style <- get_addins_style_name()
  new_style <-
    rstudioapi::showPrompt(
      "Select a style",
      "Enter the name of a style function, e.g. `styler::tidyverse_style`",
      current_style
    )
  parsed_new_style <- with_handlers(
    eval(parse(text = new_style)),
    error = function(e) {
      abort(paste0("The selected style \"", new_style, "\" is not valid: ", e$message))
    }
  )
  options(styler.addins.style = new_style)
  invisible(current_style)
}

#' Return the style function or name
#'
#' @keywords internal
get_addins_style_name <- function() {
  getOption("styler.addins.style", default = "styler::tidyverse_style")
}

#' @rdname get_addins_style_name
#' @keywords internal
get_addins_style_fun <- function() {
  eval(parse(text = get_addins_style_name()))
}

communicate_addins_style <- function() {
  style_name <- get_addins_style_name()
  cat("Using style `", style_name, "`\n", sep = "")
}
