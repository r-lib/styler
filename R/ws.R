#' @api
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import rex
NULL

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
    #remove_space_after_paren,
    #remove_space_before_paren_or_comma,
    add_space_after_comma,
    remove_extra_space_after_comma,
    remove_extra_space_before_brace,
    add_space_before_brace,
    make_add_space_around_operators(),
    remove_trailing_space,
    NULL)
  withr::with_dir(pkg_root, prettify_local(transformers))
}

#' @rdname prettify_ws
#' @export
prettify_parsed <- function(pkg = ".") {
  pkg_root <- rprojroot::find_package_root_file(path = pkg)
  transformers <- c(
    add_space_around_equal,
    fix_quotes,
    NULL)
  withr::with_dir(pkg_root, prettify_local(transformers))
}

prettify_local <- function(transformers) {
  #files <- dir(path = "R", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)
  files <- dir(path = "tests/testthat", pattern = "[.][rR]$", recursive = TRUE, full.names = TRUE)

  transform_files(files, transformers)
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
  re_substitutes_repeat(
    text,
    rex::rex(
      capture(none_of(" ")),
      one_or_more(" "),
      capture(one_of(")", ","))
    ),
    "\\1\\2")
}

remove_extra_space_after_comma <- function(text) {
  re_substitutes_repeat(
    text,
    rex::rex(
      ", ",
      one_or_more(" ")
    ),
    ", ")
}

add_space_after_comma <- function(text) {
  re_substitutes_repeat(
    text,
    rex::rex(
      ",",
      capture(none_of(" "))
    ),
    ", \\1")
}

remove_extra_space_before_brace <- function(text) {
  re_substitutes_repeat(
    text,
    rex::rex(
      capture(none_of(" ")),
      " ",
      one_or_more(" "),
      "{"
    ),
    "\\1 {")
}

add_space_before_brace <- function(text) {
  re_substitutes_repeat(
    text,
    rex::rex(
      capture(none_of(" ", "(")),
      "{"
    ),
    "\\1)")
}

convert_to_double_quotes <- function(text) {
  stop("NYI")
}

make_add_space_around_operators <- function() {
  operators <- list(
    "%/%",
    "%%",
    "&&",
    "||",
    "==",
    "!=",
    "<=",
    ">=",
    "<-",
    "->",
    "=" = list(
      behind = c("<", ">", "!", "="),
      ahead = "="),
    "<" = list(
      ahead = c("=", "-")),
    ">" = list(
      behind = "-",
      ahead = "="),
    "+" = list(
      behind = c("("),
      ahead = c(0:9, ".")),
    "-" = list(
      behind = c("<", "(", "-", "["),
      ahead = c(">", 0:9, ".", "-")),
    "*",
    "/" = list(
      behind = "%",
      ahead = "%"),
    "^",
    "&" = list(
      behind = "&",
      ahead = "&"),
    "|" = list(
      behind = "|",
      ahead = "|")
  )

  unnamed <- names(operators) == ""
  names(operators)[unnamed] <- unlist(operators[unnamed])
  operators[unnamed] <- rep(list(NULL), sum(unnamed))

  unlist(Map(make_add_space_around_operator, names(operators), operators))
}

make_add_space_around_operator <- function(op, lookaround) {
  c(
    make_add_space_before_operator(op, lookaround$behind),
    make_add_space_after_operator(op, lookaround$ahead)
  )
}

make_add_space_before_operator <- function(op, lookbehind) {
  function(text) {
    re_substitutes_repeat(
      text,
      rex::rex(
        capture(none_of(c(" ", lookbehind))),
        op
      ),
      paste0("\\1 ", op))
  }
}

make_add_space_after_operator <- function(op, lookahead) {
  function(text) {
    re_substitutes_repeat(
      text,
      rex::rex(
        op,
        capture(none_of(c(" ", lookahead)))
      ),
      paste0(op, " \\1"))
  }
}

remove_trailing_space <- function(text) {
  re_substitutes_global(
    text,
    rex::rex(
      one_or_more(" "),
      end
    ),
    "")
}
