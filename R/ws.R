#' Prettify R source code
#'
#' Performs various substitutions in all `.R` files in a package.
#' Carefully examine the results after running this function!
#'
#' @param pkg Path to a (subdirectory of an) R package
#'
#' @export
prettify_ws <- function(pkg = ".") {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  transformers <- c(
    remove_space_after_paren,
    remove_space_before_paren_or_comma,
    add_space_after_comma,
    remove_extra_space_after_comma,
    remove_extra_space_before_brace,
    add_space_before_brace,
    NULL)
  withr::with_dir(pkg_root, prettify_local(transformers))
}

#' @rdname prettify_ws
#' @export
prettify_quotes <- function(pkg = ".") {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  transformers <- c(
    convert_to_double_quotes,
    NULL)
  withr::with_dir(pkg_root, prettify_local(transformers))
}

prettify_local <- function(transformers) {
  #files <- dir(path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  files <- dir(path = "tests/testthat", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)

  transform_files(files, transformers)
  invisible()
}

remove_space_after_paren <- function(text) {
  rex::re_substitutes(
    text,
    rex::rex(
      "(",
      one_or_more(" ")
    ),
    "(")
}

remove_space_before_paren_or_comma <- function(text) {
  repeat {
    old_text <- text
    text <- rex::re_substitutes(
      text,
      rex::rex(
        capture(not(" ")),
        one_or_more(" "),
        capture(one_of(")", ","))
      ),
      "\\1\\2")

    if (text == old_text)
      break
  }
  text
}

remove_extra_space_after_comma <- function(text) {
  rex::re_substitutes(
    text,
    rex::rex(
      ", ",
      one_or_more(" ")
    ),
    ", ")
}

add_space_after_comma <- function(text) {
  rex::re_substitutes(
    text,
    rex::rex(
      ",",
      capture(not(" "))
    ),
    ", \\1")
}

remove_extra_space_before_brace <- function(text) {
  rex::re_substitutes(
    text,
    rex::rex(
      capture(not(" ")),
      " ",
      one_or_more(" "),
      "{"
    ),
    "\\1 {")
}

add_space_before_brace <- function(text) {
  rex::re_substitutes(
    text,
    rex::rex(
      capture(none_of(" ", "(")),
      "{"
    ),
    "\\1)")
}

convert_to_double_quotes <- function(text) {
  rex::re_substitutes(
    text,
    rex::rex(
      start,
      capture(zero_or_more(none_of('"', "'"))),
      "'",
      capture(zero_or_more(none_of('"', "'"))),
      "'",
      capture(zero_or_more(none_of('"', "'"))),
      end
    ),
    '\\1"\\2"\\3')
}
