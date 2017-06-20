#' Run a collection of tests
#'
#' Run transformations on all *-in.R files in a test directory and compare them
#'   with their *-out.R counterpart.
#' @param test The test to run. It corresponds to a folder name in
#'   tests/testthat.
#' @param sub_test A regex pattern to further reduce the amount of test files
#'   to be tested in the test. `sub_test` must match the beginning of file
#'   names in tests/testthat. `NULL` matches all files.
#' @details Each file name that matches `test` and `sub_test` and ends with
#'   "-in.R" is considered as an input to test. Its counterpart,
#'   the reference to compare it against is the *-out.R file. It is constructed
#'   by taking the substring of the *-in.R file before the
#'   first dash and adding -out.R. This allows for multiple in.R files to
#'   share one out.R file. You could have one_line-out.R as the reference to
#'   compare one_line-random-something-stuff-in.R and
#'   one_line-random-but-not-so-much-in.R.
#'
#'   This also implies that -out.R files cannot have more than one dash in
#'   their name, i.e. just the one before out.R.
#' @inheritParams transform_and_check
#' @importFrom purrr flatten_chr pwalk map
test_collection <- function(test, sub_test = NULL,
                            write_back = FALSE,
                            transformer) {
  path <- rprojroot::find_testthat_root_file(test)

  pattern <- if_else(!is.null(sub_test),
                     paste0("^", sub_test, ".*", "in\\.R$"),
                     "in\\.R$")

  in_names <- list.files(file.path(path),
                          pattern = pattern,
                          full.names = FALSE)

  if (length(in_names) < 1) stop("no items to check")

  out_names <- in_names %>%
    strsplit("-") %>%
    map(1) %>%
    flatten_chr() %>%
    paste0("-out.R")

  out_items <- file.path(path, out_names)
  in_items <- file.path(path, in_names)

  pwalk(list(in_items, out_items, in_names, out_names),
       transform_and_check,
       transformer = transformer, write_back = write_back)
}


#' Transform a file an check the result
#'
#' Transform an file and check whether it is identical to a reference.
#' @param in_item An path to an file to transform.
#' @param out_item The path to a file that contains the expected result.
#' @param in_name The label of the in_item, defaults to `in_item`.
#' @param out_name The label of the out_item, defaults to `out_item`.
#' @param transformer A function to apply to the content of `in_item`.
#' @param write_back Whether the results of the transformation should be written
#'   to the output file.
transform_and_check <- function(in_item, out_item,
                                in_name = in_item, out_name = out_item,
                                transformer, write_back) {
  transformed <- utf8::read_lines_enc(in_item) %>%
    transformer()
  transformed <- suppressMessages(utf8::transform_lines_enc(out_item,
    function(x) transformed,
    write_back = write_back))

  if (transformed) {
    target <- utf8::read_lines_enc(out_item)
    warning(in_name, " was different from ", out_name,
            immediate. = TRUE, call. = FALSE)
  } else {
    message(in_name, " was identical to ", out_name,
            immediate. = TRUE, call. = FALSE)
  }
}


##  ............................................................................
##  transformer functions                                                   ####

#' Transforming test input with a transformer function
#'
#' These functions can be used as inputs for [test_collection()] and
#'  [transform_and_check()].
#' @name test_transformer
#' @param text A character vector to transform.
#' @details `style_indent_round` is suited for internal tests of indention based
#'   on round brackets.
#'
#'   As inputs for [test_collection()], we can also use top-level functions such
#'   as [style_text()].

#' @rdname test_transformer
style_indent_round <- function(text) {
  text %>%
    compute_parse_data_nested() %>%
    visit(funs = c(create_filler,
                   partial(indent_round, indent_by = 2),
                   strip_eol_spaces)) %>%

    serialize_parse_data_nested()
}
