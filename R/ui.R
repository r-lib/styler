#' @api
#' @import tibble
#' @importFrom magrittr %>%
NULL

#' Prettify R source code
#'
#' Performs various substitutions in all `.R` files in a package
#' (code and tests). One can also (optionally) style `.Rmd` and/or
#' `.Rnw` files (vignettes and readme) by changing the `filetype` argument.
#' Carefully examine the results after running this function!
#'
#' @param pkg Path to a (subdirectory of an) R package.
#' @param ... Arguments passed on to the `style` function.
#' @param style A function that creates a style guide to use, by default
#'   [tidyverse_style()] (without the parentheses). Not used
#'   further except to construct the argument `transformers`. See
#'   [style_guides()] for details.
#' @param transformers A set of transformer functions. This argument is most
#'   conveniently constructed via the `style` argument and `...`. See
#'   'Examples'.
#' @param filetype Vector of file extensions indicating which file types should
#'   be styled. Case is ignored, and the `.` is optional, e.g. `c(".R", ".Rmd",
#'   ".Rnw")` or `c("r", "rmd", "rnw")`.
#' @param exclude_files Character vector with paths to files that should be
#'   excluded from styling.
#' @param include_roxygen_examples Whether or not to style code in roxygen
#'   examples.
#' @section Warning:
#' This function overwrites files (if styling results in a change of the
#' code to be formatted). It is strongly suggested to only style files
#' that are under version control or to create a backup copy.
#'
#' We suggest to first style with `scope < "tokens"` and inspect and commit
#' changes, because these changes are guaranteed to leave the abstract syntax
#' tree (AST) unchanged. See section 'Roundtrip Validation' for details.
#'
#' Then, we suggest to style with `scope = "tokens"` (if desired) and carefully
#' inspect the changes to make sure the AST is not changed in an unexpected way
#' that invalidates code.
#' @section Roundtrip Validation:
#' The following section describes when and how styling is guaranteed to
#' yield correct code.
#'
#' If the style guide has `scope < "tokens"`, no tokens are changed and the
#' abstract syntax tree (AST) should not change.
#' Hence, it is possible to validate the styling by comparing whether the parsed
#' expression before and after styling have the same AST.
#' This comparison omits comments. styler compares
#' error if the AST has changed through styling.
#'
#' Note that with `scope = "tokens"` such a comparison is not conducted because
#' the AST might well change and such a change is intended. There is no way
#' styler can validate styling, that is why we inform the user to carefully
#' inspect the changes.
#'
#' See section 'Warning' for a good strategy to apply styling safely.
#' @inheritSection transform_files Value
#' @family stylers
#' @examples
#' \dontrun{
#'
#' style_pkg(style = tidyverse_style, strict = TRUE)
#' style_pkg(
#'   scope = "line_breaks",
#'   math_token_spacing = specify_math_token_spacing(zero = "'+'")
#' )
#' }
#' @export
style_pkg <- function(pkg = ".",
                      ...,
                      style = tidyverse_style,
                      transformers = style(...),
                      filetype = "R",
                      exclude_files = "R/RcppExports.R",
                      include_roxygen_examples = TRUE) {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  changed <- withr::with_dir(pkg_root, prettify_pkg(
    transformers, filetype, exclude_files, include_roxygen_examples
  ))
  invisible(changed)
}

prettify_pkg <- function(transformers,
                         filetype,
                         exclude_files,
                         include_roxygen_examples) {
  filetype <- set_and_assert_arg_filetype(filetype)
  r_files <- vignette_files <- readme <- NULL

  if ("\\.r" %in% filetype) {
    r_files <- dir(
      path = c("R", "tests", "data-raw", "demo"), pattern = "\\.r$",
      ignore.case = TRUE, recursive = TRUE, full.names = TRUE
    )
  }

  if ("\\.rmd" %in% filetype) {
    vignette_files <- dir(
      path = "vignettes", pattern = "\\.rmd$",
      ignore.case = TRUE, recursive = TRUE, full.names = TRUE
    )
    readme <- dir(pattern = "^readme\\.rmd$", ignore.case = TRUE)
  }

  if ("\\.rnw" %in% filetype) {
    vignette_files <- append(
      vignette_files,
      dir(
        path = "vignettes", pattern = "\\.rnw$",
        ignore.case = TRUE, recursive = TRUE, full.names = TRUE
      )
    )
  }

  files <- setdiff(c(r_files, vignette_files, readme), exclude_files)
  transform_files(files, transformers, include_roxygen_examples)
}


#' Style a string
#'
#' Styles a character vector. Each element of the character vector corresponds
#' to one line of code.
#' @param text A character vector with text to style.
#' @inheritParams style_pkg
#' @family stylers
#' @examples
#' style_text("call( 1)")
#' style_text("1    + 1", strict = FALSE)
#' style_text("a%>%b", scope = "spaces")
#' style_text("a%>%b; a", scope = "line_breaks")
#' style_text("a%>%b; a", scope = "tokens")
#' # the following is identical but the former is more convenient:
#' style_text("a<-3++1", style = tidyverse_style, strict = TRUE)
#' style_text("a<-3++1", transformers = tidyverse_style(strict = TRUE))
#' @export
style_text <- function(text,
                       ...,
                       style = tidyverse_style,
                       transformers = style(...),
                       include_roxygen_examples = TRUE) {
  transformer <- make_transformer(transformers, include_roxygen_examples)
  styled_text <- transformer(text)
  construct_vertical(styled_text)
}

#' Prettify arbitrary R code
#'
#' Performs various substitutions in all `.R`, `.Rmd` and/or `.Rnw` files
#' in a directory (by default only `.R` files are styled - see `filetype` argument).
#' Carefully examine the results after running this function!
#' @param path Path to a directory with files to transform.
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   of `path` should be styled as well.
#' @inheritParams style_pkg
#' @inheritSection transform_files Value
#' @inheritSection style_pkg Warning
#' @inheritSection style_pkg Roundtrip Validation
#' @family stylers
#' @examples
#' \dontrun{
#' style_dir(file_type = "r")
#' }
#' @export
style_dir <- function(path = ".",
                      ...,
                      style = tidyverse_style,
                      transformers = style(...),
                      filetype = "R",
                      recursive = TRUE,
                      exclude_files = NULL,
                      include_roxygen_examples = TRUE) {
  changed <- withr::with_dir(
    path, prettify_any(
      transformers, filetype, recursive, exclude_files, include_roxygen_examples
    )
  )
  invisible(changed)
}

#' Prettify R code in current working directory
#'
#' This is a helper function for style_dir.
#' @inheritParams style_pkg
#' @param recursive A logical value indicating whether or not files in subdirectories
#'   should be styled as well.
#' @keywords internal
prettify_any <- function(transformers,
                         filetype,
                         recursive,
                         exclude_files,
                         include_roxygen_examples) {
  files <- dir(
    path = ".", pattern = map_filetype_to_pattern(filetype),
    ignore.case = TRUE, recursive = recursive, full.names = TRUE
  )
  transform_files(
    setdiff(files, exclude_files), transformers, include_roxygen_examples
  )
}

#' Style `.R`, `.Rmd` or `.Rnw` files
#'
#' Performs various substitutions in the files specified.
#' Carefully examine the results after running this function!
#' @section Encoding:
#' UTF-8 encoding is assumed. Please convert your code to UTF-8 if necessary
#' before applying styler.
#' @param path A character vector with paths to files to style.
#' @inheritParams style_pkg
#' @inheritSection transform_files Value
#' @inheritSection style_pkg Warning
#' @inheritSection style_pkg Roundtrip Validation
#' @examples
#' # the following is identical but the former is more convenient:
#' file <- tempfile("styler", fileext = ".R")
#' xfun::write_utf8("1++1", file)
#' style_file(file, style = tidyverse_style, strict = TRUE)
#' style_file(file, transformers = tidyverse_style(strict = TRUE))
#' xfun::read_utf8(file)
#' unlink(file)
#' @family stylers
#' @export
style_file <- function(path,
                       ...,
                       style = tidyverse_style,
                       transformers = style(...),
                       include_roxygen_examples = TRUE) {
  changed <- withr::with_dir(
    dirname(path),
    transform_files(basename(path), transformers, include_roxygen_examples)
  )
  invisible(changed)
}
