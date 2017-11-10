#' Run a collection of tests
#'
#' Run transformations on all *-in.R files in a test directory and compare them
#'   with their *-out.R counterpart.
#' @inheritParams transform_and_check
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
                            write_back = TRUE,
                            write_tree = NA,
                            transformer,
                            ...) {
  path <- rprojroot::find_testthat_root_file(test)

  pattern <- if_else(!is.null(sub_test),
                     paste0("^", sub_test, ".*", "in\\.R$"),
                     "in\\.R$")

  in_names <- list.files(file.path(path),
                          pattern = pattern,
                          full.names = FALSE)

  if (length(in_names) < 1) stop("no items to check")

  out_names <- construct_out(in_names)

  out_items <- file.path(path, out_names)
  in_items <- file.path(path, in_names)

  out_trees <- construct_tree(in_items)

  pwalk(list(in_items, out_items, in_names, out_names, out_trees),
       transform_and_check,
       transformer = transformer,
       write_back = write_back,
       write_tree = write_tree,
       ...)
}

#' Construct *-out.R from a *-in.R
#'
#' Multiple *-in.R files can have the same *-out.R file since to create the
#'   *-out.R file, everything after the first dash is replaced by *-out.R.
#' @param in_paths A character vector that denotes paths to *-in.R files.
#' @examples
#' styler:::construct_out(c("path/to/file/first-in.R",
#'  "path/to/file/first-extended-in.R"))
construct_out <- function(in_paths) {
  gsub("\\-.*$", "\\-out\\.R", in_paths)
}

#' Construct paths of a tree object given the paths of *-in.R files
#'
#' @param in_paths Character vector of *-in.R files.
#' @param suffix Suffix for the tree object.
construct_tree <- function(in_paths, suffix = "_tree") {
  gsub("\\.R$", suffix, in_paths)
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
#' @param write_tree Whether or not the tree structure of the test should be
#'   computed and written to a file. Note that this needs R >= 3.2
#'   (see [set_arg_write_tree()]). If the argument is set to `NA`, the function
#'   determines whether R >= 3.2 is in use and if so, trees will be written.
#' @param ... Parameters passed to transformer function.
#' @param out_tree Name of tree file if written out.
#' @importFrom utils write.table
transform_and_check <- function(in_item, out_item,
                                in_name = in_item, out_name = out_item,
                                transformer, write_back,
                                write_tree = NA,
                                out_tree = "_tree", ...) {

  write_tree <- set_arg_write_tree(write_tree)
  read_in <- enc::read_lines_enc(in_item)
  if (write_tree) {
    create_tree(read_in) %>%
      write.table(out_tree, col.names = FALSE, row.names = FALSE, quote = FALSE)
  }
  transformed_text <- read_in %>%
    transformer(...) %>%
    unclass()
  transformed <- enc::transform_lines_enc(
    out_item,
    function(x) transformed_text,
    write_back = write_back,
    verbose = FALSE
  )

  if (transformed) {
    target <- enc::read_lines_enc(out_item)
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
#' @details
#'   As inputs for [test_collection()], we can also use top-level functions such
#'   as [style_text()].
#' @rdname test_transformer
NULL

#' @describeIn test_transformer Nest and unnest `text` without applying any
#'   transformations but remove EOL spaces and indention due to the way the
#'   serialization is set up.
style_empty <- function(text) {
  transformers <- list(
    # transformer functions
    initialize = initialize_attributes,
    line_break = NULL,
    space      = NULL,
    token      = NULL,

    # transformer options
    use_raw_indention = FALSE,
    reindention = specify_reindention(),
    NULL
  )
  transformed_text <- parse_transform_serialize(text, transformers)
  transformed_text
}

#' @describeIn test_transformer Transformations for indention based on operators
style_op <- function(text) {

  transformers <- list(
    # transformer functions
    initialize = initialize_attributes,
    line_break = NULL,
    space      = partial(indent_op, indent_by = 2),
    token      = NULL,

    # transformer options
    use_raw_indention = FALSE,
    reindention = specify_reindention(),
    NULL
  )

  transformed_text <- parse_transform_serialize(text, transformers)
  transformed_text

}

#' Create the path to a test that file
#' @param ... Arguments passed to [file.path()] to construct the path after
#'   ".../tests/testthat/"
testthat_file <- function(...) {
  file.path(rprojroot::find_testthat_root_file(), ...)
}

#' Set arguments
#' @param write_tree Whether or not to write tree.
#' @name set_args
NULL

#' @describeIn set_args Sets the argument `write_tree` in
#'   [test_collection()] to be `TRUE` for R versions higher or equal to 3.2, and
#'   `FALSE` otherwise since the second-level dependency `DiagrammeR` from
#'   `data.table` is not available for R < 3.2.
set_arg_write_tree <- function(write_tree) {
  sufficient_version <- getRversion() >= 3.2
  if (is.na(write_tree)) {
    write_tree <- ifelse(sufficient_version, TRUE, FALSE)
  } else if (!sufficient_version & write_tree) {
    stop_insufficient_r_version()
  }
  write_tree
}

stop_insufficient_r_version <- function() {
  stop(paste0(
    "Can't write tree with R version ", getRversion(),
    "since data.tree not available. Needs at least R version 3.2."
  ), call. = FALSE)
}
