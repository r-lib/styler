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
#' @details
#' Each file name that matches `test` and `sub_test` and ends with
#' "-in.R" is considered as an input to test. Its counterpart,
#' the reference to compare it against is the *-out.R file. It is constructed
#' by taking the substring of the *-in.R file before the
#' last dash and adding -out.R. In contrast to older versions of this
#' function, every *-out.R file has just one in file.
#' @inheritParams transform_and_check
#' @importFrom purrr flatten_chr pwalk map
#' @importFrom rlang abort
#' @keywords internal
test_collection <- function(test, sub_test = NULL,
                            write_back = TRUE,
                            write_tree = NA,
                            transformer,
                            ...) {
  path <- rprojroot::find_testthat_root_file(test)

  pattern <- paste0(
    if (!is.null(sub_test)) paste0("^", sub_test, ".*"),
    "in\\.R(?:|md|nw)$"
  )

  in_names <- list.files(
    file.path(path),
    pattern = pattern,
    full.names = FALSE
  )

  if (length(in_names) < 1) abort("no items to check")

  out_names <- construct_out(in_names)

  out_items <- file.path(path, out_names)
  in_items <- file.path(path, in_names)

  out_trees <- construct_tree(in_items)

  pwalk(list(in_items, out_items, in_names, out_names, out_trees),
    transform_and_check,
    transformer = transformer,
    write_back = write_back,
    write_tree = write_tree,
    ...
  )
}

#' Construct *-out.R from a *-in.R
#'
#' Multiple *-in.R files can have the same *-out.R file since to create the
#'   *-out.R file, everything after the first dash is replaced by *-out.R.
#' @param in_paths A character vector that denotes paths to *-in.R files.
#' @examples
#' styler:::construct_out(c(
#'   "path/to/file/first-in.R",
#'   "path/to/file/first-extended-in.R"
#' ))
#' @keywords internal
construct_out <- function(in_paths) {
  gsub("\\-in([.]R(?:|md|nw))$", "\\-out\\1", in_paths)
}

#' Construct paths of a tree object given the paths of *-in.R files
#'
#' @param in_paths Character vector of *-in.R files.
#' @param suffix Suffix for the tree object.
#' @keywords internal
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
#' @importFrom rlang warn
#' @keywords internal
transform_and_check <- function(in_item, out_item,
                                in_name = in_item, out_name = out_item,
                                transformer, write_back,
                                write_tree = NA,
                                out_tree = "_tree", ...) {
  write_tree <- set_arg_write_tree(write_tree)
  read_in <- xfun::read_utf8(in_item)
  if (write_tree) {
    create_tree(read_in) %>%
      write.table(out_tree, col.names = FALSE, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
  }
  transformed_text <- read_in %>%
    transformer(...) %>%
    unclass()
  if (!file.exists(out_item)) {
    warn(paste(
      "File", out_item, "does not exist. Creating it from transormation."
    ))
    file.create(out_item)
  }
  transformed <- transform_utf8(
    out_item,
    function(x) transformed_text,
    write_back = write_back
  )

  if (transformed) {
    warn(paste(in_name, "was different from", out_name))
  } else {
    message(in_name, " was identical to ", out_name)
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
#' @keywords internal
NULL

#' @describeIn test_transformer Nest and unnest `text` without applying any
#'   transformations but remove EOL spaces and indention due to the way the
#'   serialization is set up.
#' @keywords internal
style_empty <- function(text) {
  transformers <- list(
    # transformer functions
    initialize = default_style_guide_attributes,
    line_break        = NULL,
    space             = NULL,
    token             = NULL,
    # transformer options
    use_raw_indention = FALSE,
    reindention       = specify_reindention(),
    NULL
  )
  transformed_text <- parse_transform_serialize_r(text, transformers)
  transformed_text
}

#' @describeIn test_transformer Transformations for indention based on operators
#' @keywords internal
style_op <- function(text) {
  transformers <- list(
    # transformer functions
    initialize        = default_style_guide_attributes,
    line_break        = NULL,
    space             = partial(indent_op, indent_by = 2),
    token             = NULL,
    # transformer options
    use_raw_indention = FALSE,
    reindention       = specify_reindention(),
    NULL
  )

  transformed_text <- parse_transform_serialize_r(text, transformers)
  transformed_text
}


#' Create the path to a test that file
#' @param ... Arguments passed to [file.path()] to construct the path after
#'   ".../tests/testthat/"
#' @keywords internal
testthat_file <- function(...) {
  file.path(rprojroot::find_testthat_root_file(), ...)
}

#' Convert a serialized R object to a certaion verion.
#'
#' Needed to make [testthat::expect_known_value()] work on R < 3.6.
#' @param path A path to an rds file.
#' @param version The target version.
#' @keywords internal
rds_to_version <- function(path, version = 2) {
  readRDS(path) %>%
    saveRDS(path, version = version)
}



#' Copy a file to a temporary directory
#'
#' Takes the path to a file as input and returns the path where the temporary
#' file is stored. Don't forget to unlink once you are done.
#' @param path_perm The path of the file to copy.
#' @keywords internal
copy_to_tempdir <- function(path_perm = testthat_file()) {
  dir <- tempfile("styler")
  dir.create(dir)
  file.copy(path_perm, dir)
  base <- basename(path_perm)
  file.path(dir, base)
}

#' Generate a comprehensive collection test cases for comment / insertion
#' interaction
#' Test consist of if / if-else / if-else-if-else cases, paired with various
#' line-break and comment configurations. Used for internal testing.
#' @return
#' The function is called for its side effects, i.e. to write the
#' test cases to *-in.R files that can be tested with [test_collection()]. Note
#' that a few of the test cases are invalid and need to be removed / commented
#' out manually.
#' @keywords internal
generate_test_samples <- function() {
  gen <- function(x) {
    if (length(x) == 0) {
      ""
    } else {
      c(
        paste0(x[1], gen(x[-1])),
        paste0(x[1], " # comment\n", paste(x[-1], collapse = ""))
      )
    }
  }

  collapse <- function(x) paste(x, collapse = "\n\n")

  cat(
    collapse(gen(c("if", "(", "TRUE", ")", "NULL"))),
    file = "tests/testthat/insertion_comment_interaction/just_if-in.R"
  )
  cat(
    collapse(gen(c("if", "(", "TRUE", ")", "NULL", " else", " NULL"))),
    file = "tests/testthat/insertion_comment_interaction/if_else-in.R"
  )
  cat(
    collapse(gen(c(
      "if", "(", "TRUE", ")", "NULL", " else", " if", "(", "FALSE", ")", "NULL",
      " else", " NULL"
    ))),
    file = "tests/testthat/insertion_comment_interaction/if_else_if_else-in.R"
  )
}
