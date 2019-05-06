#' Work with parser versions
#'
#' The structure of the parse data affects many operations in styler. There was
#' unexpected behavior of the parser that styler was initially designed to work
#' around. Examples are [#187](https://github.com/r-lib/styler/issues/187),
#' [#216](https://github.com/r-lib/styler/issues/216),
#' [#100](https://github.com/r-lib/styler/issues/100) and others. With
#' [#419](https://github.com/r-lib/styler/issues/419), the structure of the parse
#' data changes and we need to dispatch for older versions. As it is inconvenient
#' to pass a parser version down in the call stack in various places, the
#' environment `env_current` is used to store the current version *globally*
#' but internally.
#'
#' We version the parser as follows:
#'
#' * version 1: Before fix mentioned in #419.
#' * version 2: After #419.
#'
#' The following utilities are available:
#'
#' * `parser_version_set()` sets the parser version in the environment
#'   `env_current`.
#' * `parser_version_get()` retrieves the parser version from the
#'   environment `env_current`.
#' * `parser_version_find()` determines the version of the parser from parse
#'   data. This does not necessarily mean that the version found is the
#'   actual version, but it *behaves* like it. For example, code that does not
#'   contain `EQ_ASSIGN` is parsed the same way with version 1 and 2. If the
#'   behavior is identical, the version is set to 1.
#' @param version The version of the parser to be used.
#' @param pd A parse table such as the output from
#'   `utils::getParseData(parse(text = text))`.
#' @keywords internal
parser_version_set <- function(version) {
  env_current$parser_version <- version
}

#' @rdname parser_version_set
parser_version_get <- function() {
  env_current$parser_version
}

#' @rdname parser_version_set
parser_version_find <- function(pd) {
  ifelse(any(pd$token == "equal_assign"), 2, 1)
}



env_current <- rlang::new_environment(parent = rlang::empty_env())
