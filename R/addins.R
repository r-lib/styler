#' Stylers for RStudio Addins
#'
#' Helper functions for styling via RStudio Addins.
#' @section Addins:
#' - Set style: Select the style transformers to use. For flexibility, the user
#'   input is passed to the `transformers` argument, not the `style` argument,
#'   so entering `styler::tidyverse_style(scope = "spaces")` in the Addin is
#'   equivalent to `styler::style_text("1+1", scope = "spaces")` and
#'   `styler::style_text("1+1", transformers = styler::tidyverse_style(scope = "spaces"))`
#'   if the text to style is `1+1`. The style transformers are memorized
#'   within an R session via the R option `styler.addins_style_transformer` so
#'   if you want it to persist over sessions, set the option
#'   `styler.addins_style_transformer` in your `.Rprofile`.
#' - Style active file: Styles the active file, by default with
#'   [tidyverse_style()] or the value of the option
#'   `styler.addins_style_transformer` if specified.
#' - Style selection: Same as *Style active file*, but styles the highlighted
#'   code instead of the whole file.
#' @section Auto-Save Option:
#' By default, both of the RStudio Addins will apply styling to the (selected)
#' file contents without saving changes. Automatic saving can be enabled by
#' setting the R option `styler.save_after_styling` to `TRUE`.
#' Consider setting this in your `.Rprofile` file if you want to persist
#' this setting across multiple sessions. Untitled files will always need to be
#' saved manually after styling.
#' @section Life cycle:
#' The way of specifying the style in the Addin as well as the auto-save option
#' (see below) are experimental. We are currently considering letting the user
#' specify the defaults for other style APIs like [styler::style_text()],
#' either via R options, config files or other ways as well.
#' See [r-lib/styler#319](https://github.com/r-lib/styler/issues/319) for
#' the current status of this.
#' @name styler_addins
#' @family stylers
#' @examples
#' \dontrun{
#' # save after styling when using the Addin
#' options(styler.save_after_styling = TRUE)
#' # only style with scope = "spaces" when using the Addin
#' val <- "styler::tidyverse_style(scope = 'spaces')"
#' options(
#'   styler.addins_style_transformer = val
#' )
#' }
NULL



#' @keywords internal
style_active_file <- function() {
  communicate_addins_style_transformers()
  context <- get_rstudio_context()
  transformer <- make_transformer(get_addins_style_transformer(),
    include_roxygen_examples = TRUE,
    base_indention = 0L,
    warn_empty = is_plain_r_file(context$path)
  )
  is_r_file <- any(
    is_plain_r_file(context$path),
    is_unsaved_file(context$path),
    is_rprofile_file(context$path)
  )

  if (is_rmd_file(context$path) || is_qmd_file(context$path)) {
    out <- transform_mixed(context$contents, transformer, filetype = "Rmd")
  } else if (is_rnw_file(context$path)) {
    out <- transform_mixed(context$contents, transformer, filetype = "Rnw")
  } else if (is_r_file) {
    out <- try_transform_as_r_file(context, transformer)
  } else {
    abort("Can only style .qmd, .R, .Rmd, and .Rnw files.")
  }
  rstudioapi::modifyRange(
    c(1L, 1L, length(context$contents) + 1L, 1L),
    paste(ensure_last_n_empty(out), collapse = "\n"),
    id = context$id
  )
  if (save_after_styling_is_active() && context$path != "") {
    rstudioapi::documentSave(context$id)
  }
  rstudioapi::setCursorPosition(context$selection[[1L]]$range)
}

#' Wrapper around [style_pkg()] for access via Addin.
#' @keywords internal
style_active_pkg <- function() {
  communicate_addins_style_transformers()
  style_pkg(transformers = get_addins_style_transformer())
}

#' Heuristic to see if a file styled with the addin should be saved or not.
#'
#' Using the R option `"styler.save_after_styling"` and if unset, checks legacy
#' method via environment variable `save_after_styling`.
#' @keywords internal
save_after_styling_is_active <- function() {
  op_old <- as.logical(toupper(Sys.getenv("save_after_styling")))
  op_new <- getOption("styler.save_after_styling", default = "")
  if (!is.na(op_old)) {
    cli::cli_warn(c(
      "Using the environment variable {.envvar save_after_styling} is \\
      deprecated and won't work in a future version of styler. ",
      "!" = "Please use `options(styler.save_after_styling)` \\
             to control the behavior.",
      i = "If both are set, the R option is used."
    ))
  }

  if (op_new == "") {
    if (is.na(op_old)) {
      op <- FALSE
    } else {
      op <- op_old
    }
  } else {
    op <- op_new
  }
  op
}

#' Styles the highlighted selection in a `.R` or `.Rmd` file.
#' @keywords internal
style_selection <- function() {
  communicate_addins_style_transformers()
  context <- get_rstudio_context()
  text <- context$selection[[1L]]$text
  if (!any(nzchar(text))) abort("No code selected")
  out <- style_text(
    text,
    transformers = get_addins_style_transformer(),
    base_indention = nchar(gsub("^( *).*", "\\1", text))
  )
  rstudioapi::modifyRange(
    context$selection[[1L]]$range,
    paste(c(
      out,
      if (context$selection[[1L]]$range$end[2L] == 1L) ""
    ), collapse = "\n"),
    id = context$id
  )
  if (save_after_styling_is_active() && context$path != "") {
    invisible(rstudioapi::documentSave(context$id))
  }
}

get_rstudio_context <- function() {
  rstudioapi::getActiveDocumentContext()
}

#' Asks the user to supply a style
#' @keywords internal
set_style_transformers <- function() {
  current_style <- get_addins_style_transformer_name()
  new_style <-
    rstudioapi::showPrompt(
      "Select a style",
      "Enter the name of a style transformer, e.g. `styler::tidyverse_style()`",
      current_style
    )
  if (!is.null(new_style)) {
    parsed_new_style <- rlang::try_fetch(
      {
        transformers <- eval(parse(text = new_style))
        style_text(
          c("a = 2", "function() {", "NULL", "}"),
          transformers = transformers
        )
      },
      error = function(e) {
        abort(paste0(
          "The selected style transformers \"",
          new_style, "\" is not valid: ", e$message
        ))
      }
    )
    options(styler.addins_style_transformer = new_style)
  }

  invisible(current_style)
}

#' Return the style function or name
#' @keywords internal
get_addins_style_transformer_name <- function() {
  getOption("styler.addins_style_transformer")
}

#' @rdname get_addins_style_transformer_name
#' @keywords internal
get_addins_style_transformer <- function() {
  eval(parse(text = get_addins_style_transformer_name()))
}

communicate_addins_style_transformers <- function() {
  style_name <- get_addins_style_transformer_name()
  if (!getOption("styler.quiet", FALSE)) {
    cat("Using style transformers `", style_name, "`\n", sep = "")
  }
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
#' @keywords internal
try_transform_as_r_file <- function(context, transformer) {
  rlang::try_fetch(
    transformer(context$contents),
    error = function(e) {
      preamble_for_unsaved <- paste(
        "Styling of unsaved files is only supported for R files with valid",
        "code. Please save the file (as .qmd, .R, or .Rmd) and make sure that",
        "the R code in it can be parsed. Then, try to style again."
      )

      if (context$path == "") {
        abort(paste0(preamble_for_unsaved, " The error was \n", e$message))
      } else {
        abort(e$message)
      }
    }
  )
}
