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
                            dry = "off",
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

  if (getOption("styler.test_dir_writable", TRUE)) {
    out_items <- file.path(path, out_names)
    in_items <- file.path(path, in_names)
    out_trees <- construct_tree(in_items)
  } else {
    in_items <- file.path(path, in_names)
    out_items <- file.path(tempdir(), out_names)
    ref_items <- file.path(path, out_names)
    file.copy(ref_items, out_items, overwrite = TRUE, copy.mode = FALSE)
    out_trees <- file.path(tempdir(), construct_tree(in_names))
  }

  pwalk(list(in_items, out_items, in_names, out_names, out_trees),
    transform_and_check,
    transformer = transformer,
    dry = dry,
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
#' @param write_tree Whether or not the tree structure of the test should be
#'   computed and written to a file. Note that this needs R >= 3.2
#'   (see [set_arg_write_tree()]). If the argument is set to `NA`, the function
#'   determines whether R >= 3.2 is in use and if so, trees will be written.
#' @param ... Parameters passed to transformer function.
#' @param out_tree Name of tree file if written out.
#' @inheritParams transform_utf8
#' @importFrom utils write.table
#' @importFrom rlang warn
#' @keywords internal
transform_and_check <- function(in_item, out_item,
                                in_name = in_item, out_name = out_item,
                                transformer, dry,
                                write_tree = NA,
                                out_tree = "_tree", ...) {
  write_tree <- set_arg_write_tree(write_tree)
  read_in <- xfun::read_utf8(in_item)
  if (write_tree) {
    create_tree(read_in) %>%
      write.table(out_tree,
        col.names = FALSE, row.names = FALSE, quote = FALSE,
        fileEncoding = "UTF-8"
      )
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
    dry = dry
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
style_empty <- function(text, base_indention = 0) {
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
  transformed_text <- parse_transform_serialize_r(text,
    transformers = transformers,
    base_indention = base_indention
  )
  transformed_text
}

#' @describeIn test_transformer Transformations for indention based on operators
#' @keywords internal
style_op <- function(text, base_indention = 0) {
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

  transformed_text <- parse_transform_serialize_r(text,
    transformers = transformers,
    base_indention = base_indention
  )
  transformed_text
}


#' Create the path to a test that file
#' @param ... Arguments passed to [file.path()] to construct the path after
#'   ".../tests/testthat/"
#' @keywords internal
testthat_file <- function(...) {
  file.path(rprojroot::find_testthat_root_file(), ...)
}

#' Convert a serialized R object to a certain version.
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

#' Times two function calls with temporarily enabled cache
#'
#' This can be helpful for benchmarking.
#' @param ... Arguments passed to `fun`.
#' @param fun The function that should be timed.
#' @param n The number of times the experiment should be repeated.
#' @return
#' A scalar indicating the relative difference of the second compared to the
#'   first run.
#' @keywords internal
n_times_faster_with_cache <- function(x1, x2 = x1, ...,
                                      fun = styler::style_text,
                                      n = 3,
                                      clear = "always") {
  rlang::arg_match(clear, c("always", "final", "never", "all but last"))

  out <- purrr::map(1:n, n_times_faster_bench,
    x1 = x1, x2 = x2, fun = fun,
    ..., n = n, clear = clear
  ) %>%
    purrr::map_dbl(
      ~ unname(.x$first["elapsed"] / .x$second["elapsed"])
    ) %>%
    mean()

  if (clear %in% c("always", "final")) {
    clear_testthat_cache()
  }
  out
}


n_times_faster_bench <- function(i, x1, x2, fun, ..., n, clear) {
  fresh_testthat_cache()
  if ((clear == "always") || (clear == "all but last" & n != i)) {
    on.exit(clear_testthat_cache())
  }
  first <- system.time(fun(x1, ...))

  if (is.null(x2)) {
    second <- c(elapsed = 1)
  } else {
    second <- system.time(fun(x2, ...))
  }
  list(
    first = first,
    second = second
  )
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

#' @include ui-caching.R
clear_testthat_cache <- purrr::partial(cache_clear, "testthat", ask = FALSE)
activate_testthat_cache <- purrr::partial(cache_activate, "testthat")

fresh_testthat_cache <- function() {
  clear_testthat_cache()
  activate_testthat_cache()
}

#' Establish testing setup for current environment
#'
#' @param testthat_cache Whether or not to create and activate a fresh testthat
#'   cache.
#' @param .local_envir The environment to use for scoping.
#' @details
#' * make styler quiet.
local_test_setup <- function(testthat_cache = FALSE,
                             .local_envir = parent.frame()) {
  ###
  withr::local_options(list("styler.quiet" = TRUE), .local_envir = .local_envir)
  if (testthat_cache) {
    fresh_testthat_cache()
    withr::defer(
      {
        clear_testthat_cache()
        cache_deactivate()
      },
      envir = .local_envir
    )
  }
}

cache_more_specs_default <- function() {
  cache_more_specs(include_roxygen_examples = TRUE, base_indention = 0)
}

#' Test `transformers_drop` for consistency
#'
#' Check if the argument `transformers_drop` in [create_style_guide()] is
#' consistent with the transformers specified in that function.
#' @param transformers The output of [create_style_guide()] we want to test.
#' @keywords internal
test_transformers_drop <- function(transformers) {
  scopes <- intersect(
    names(transformers$transformers_drop),
    names(transformers)
  )

  purrr::walk2(transformers$transformers_drop, transformers[scopes], function(x, y) {
    # all x must be in y. select the x that are not in y
    diff <- setdiff(names(x), names(y))
    if (length(diff) > 0) {
      rlang::abort(paste(
        "transformers_drop specifies exclusion rules for transformers that ",
        "are not in the style guilde. Please add the rule to the style guide ",
        "or remove the dropping rules:", paste(diff, collapse = ", ")
      ))
    }
  })
}
