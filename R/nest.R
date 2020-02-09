#' Obtain a nested parse table from a character vector
#'
#' Parses `text` to a flat parse table and subsequently changes its
#' representation into a nested parse table with [nest_parse_data()].
#' @param text A character vector to parse.
#' @return A nested parse table. See [tokenize()] for details on the columns
#'   of the parse table.
#' @importFrom purrr when
#' @keywords internal
compute_parse_data_nested <- function(text,
                                      transformers) {
  parse_data <- tokenize(text) %>%
    add_terminal_token_before() %>%
    add_terminal_token_after() %>%
    add_stylerignore() %>%
    add_attributes_caching(transformers) %>%
    drop_cached_children()

  env_add_stylerignore(parse_data)

  parse_data$child <- rep(list(NULL), length(parse_data$text))
  pd_nested <- parse_data %>%
    nest_parse_data() %>%
    flatten_operators() %>%
    when(any(parse_data$token == "EQ_ASSIGN") ~ relocate_eq_assign(.), ~.) %>%
    add_cache_block()

  pd_nested
}

#' Add the block id to a parse table
#'
#' Must be after [nest_parse_data()] because requires a nested parse table as
#' input.
#' @param pd_nested A top level nest.
#' @keywords internal
#' @importFrom rlang seq2
add_cache_block <- function(pd_nested) {
  if (cache_is_activated()) {
    pd_nested$block <- cache_find_block(pd_nested)
  } else {
    pd_nested$block <- rep(1, length(pd_nested$block))
  }
  pd_nested
}

#' Drop all children of a top level expression that are cached
#'
#' Note that we do cache top-level comments. Because package code has a lot of
#' roxygen comments and each of them is a top level expresion, checking is
#' very expensive.
#' @param pd A top-level nest.
#' @details
#' Because we process in blocks of expressions for speed, a cached expression
#' will always end up in a block that won't be styled again (usual case), unless
#' it's on a line where multiple expressions sit and at least one is not styled
#' (exception).
#'
#' **usual case: All other expressions in a block are cached**
#'
#' Cached expressions don't need to be transformed with `transformers` in
#' [parse_transform_serialize_r_block()], we simply return `text` for the top
#' level token. For that
#' reason, the nested parse table can, at the rows where these expressions are
#' located, be shallow, i.e. it does not have to contain a child, because it
#' will neither be transformed nor serialized anytime. This function drops all
#' associated tokens except the top-level token for such expressions, which will
#' result in large speed improvements in [compute_parse_data_nested()] because
#' nesting is expensive and will not be done for cached expressions.
#'
#' **exception: Not all other expressions in a block are cached**
#'
#' As described in [cache_find_block()], expressions on the same line are always
#' put into one block. If any element of a block is not cached, the block will
#' be styled as a whole. If the parse table was made shallow (and the top level)
#' expresion is still marked as non-terminal, `text` will never be used in the
#' transformation process and eventually lost. Hence, we must change the top
#' level expression to a terminal. It will act like a comment in the sense that
#' it is a fixed `text`.
#'
#' Because for the usual case, it does not even matter if the cached expression
#' is a terminal or not (because it is not processed), we can safely set
#' `terminal = TRUE` in general.
#' @section Implementation:
#' Because the structure of the parse table is not always "top-level expression
#' first, then children", this function creates a temporary parse table that has
#' this property and then extract the ids and subset the original parse table so
#' it is shallow in the right places.
#' @keywords internal
drop_cached_children <- function(pd) {
  if (cache_is_activated()) {
    pd_parent_first <- pd[order(pd$line1, pd$col1, -pd$line2, -pd$col2, as.integer(pd$terminal)), ]
    pos_ids_to_keep <- pd_parent_first %>%
      split(cumsum(pd_parent_first$parent == 0)) %>%
      map(find_pos_id_to_keep) %>%
      unlist() %>%
      unname()
    pd[pd$pos_id %in% pos_ids_to_keep, ]
  } else {
    pd
  }
}

#' Find the pos ids to keep
#'
#' To make a parse table shallow, we must know which ids to keep.
#' `split(cumsum(pd_parent_first$parent < 1))` above puts comments with negative
#' parents in the same block as proceeding expressions. `find_pos_id_to_keep()`
#' must hence always keep comments. We did not use
#' `split(cumsum(pd_parent_first$parent < 1))` because then every comment is an
#' expression on its own and processing takes much longer for typical roxygen
#' annotated code
#' @param pd A temporary top level nest where the first expression is always a
#'   top level expression, potentially cached.
#' @keywords internal
find_pos_id_to_keep <- function(pd) {
  if (pd$is_cached[1]) {
    pd$pos_id[c(TRUE, pd[-1L, "token"] == "COMMENT")]
  } else {
    pd$pos_id
  }
}


#' Turn off styling for parts of the code
#'
#' Using stylerignore markers, you can temporarily turn off styler. See a
#' few illustrative examples below.
#' @details
#' Styling is on by default when you run styler.
#' - To mark the start of a sequence where you want to turn styling off, use
#'   `# styler: off`.
#' - To mark the end of this sequence, put `# styler: on` in your code. After
#'   that line, styler will again format your code.
#' - To ignore an inline statement (i.e. just one line), place `# styler: off`
#'   at the end of the line. Note that inline statements cannot contain other
#'   comments apart from the marker, i.e. a line like
#'   `1 # comment # styler: off` won't be ignored.
#'
#' To use something else as start and stop markers, set the R options
#' `styler.ignore_start` and
#' `styler.ignore_stop` using [options()]. If you want these
#' settings to persist over multiple R sessions, consider setting them in your
#' R profile, e.g. with `usethis::edit_rprofile()`.
#' @name stylerignore
#' @examples
#' # as long as the order of the markers is correct, the lines are ignored.
#' style_text(
#'   "
#'   1+1
#'   # styler: off
#'   1+1
#'   # styler: on
#'   1+1
#'   "
#' )
#'
#' # if there is a stop marker before a start marker, styler won't be able
#' # to figure out which lines you want to ignore and won't ignore anything,
#' # issuing a warning.
#' \dontrun{
#' style_text(
#'   "
#'   1+1
#'   # styler: off
#'   1+1
#'   # styler: off
#'   1+1
#'   "
#' )
#' }
#'
NULL


#' Enhance the mapping of text to the token "SPECIAL"
#'
#' Map text corresponding to the token "SPECIAL" to a (more) unique token
#' description.
#' @param pd A parse table.
#' @keywords internal
enhance_mapping_special <- function(pd) {
  pipes <- pd$token == "SPECIAL" & pd$text == "%>%"
  pd$token[pipes] <- special_and("PIPE")

  ins <- pd$token == "SPECIAL" & pd$text == "%in%"
  pd$token[ins] <- special_and("IN")

  others <- pd$token == "SPECIAL" & !(pipes | ins)
  pd$token[others] <- special_and("OTHER")

  pd
}

special_and <- function(text) {
  paste0("SPECIAL-", text)
}

#' Add information about previous / next token to each terminal
#'
#' @param pd_flat A flat parse table.
#' @name add_token_terminal
#' @keywords internal
NULL

#' @rdname add_token_terminal
#' @keywords internal
add_terminal_token_after <- function(pd_flat) {
  terminals <- pd_flat %>%
    filter(terminal) %>%
    arrange_pos_id()

  new_tibble(list(
    pos_id = terminals$pos_id,
    token_after = lead(terminals$token, default = "")
  ),
  nrow = nrow(terminals)
  ) %>%
    left_join(pd_flat, ., by = "pos_id")
}

#' @rdname add_token_terminal
#' @keywords internal
add_terminal_token_before <- function(pd_flat) {
  terminals <- pd_flat %>%
    filter(terminal) %>%
    arrange_pos_id()

  new_tibble(
    list(
      id = terminals$id,
      token_before = lag(terminals$token, default = "")
    ),
    nrow = nrow(terminals)
  ) %>%
    left_join(pd_flat, ., by = "id")
}

#' Initialise variables related to caching
#'
#' @param transformers A list with transformer functions, used to check if
#'   the code is cached.
#' @describeIn add_token_terminal Initializes `newlines` and `lag_newlines`.
#' @keywords internal
add_attributes_caching <- function(pd_flat, transformers) {
  pd_flat$block <- pd_flat$is_cached <- rep(NA, nrow(pd_flat))
  if (cache_is_activated()) {
    is_parent <- pd_flat$parent == 0
    pd_flat$is_cached[is_parent] <- map_lgl(
      pd_flat$text[pd_flat$parent == 0],
      is_cached, transformers
    )
    is_comment <- pd_flat$token == "COMMENT"
    pd_flat$is_cached[is_comment] <- rep(FALSE, sum(is_comment))
  }
  pd_flat
}

#' @describeIn add_token_terminal Removes column `terimnal_token_before`. Might
#'   be used to prevent the use of invalidated information, e.g. if tokens were
#'   added to the nested parse table.
#' @keywords internal
remove_terminal_token_before_and_after <- function(pd_flat) {
  pd_flat$token_before <- NULL
  pd_flat$token_after <- NULL
  pd_flat
}

#' Helper for setting spaces
#'
#' @param spaces_after_prefix An integer vector with the number of spaces
#'   after the prefix.
#' @param force_one Whether spaces_after_prefix should be set to one in all
#'   cases.
#' @return An integer vector of length spaces_after_prefix, which is either
#'   one (if `force_one = TRUE`) or `space_after_prefix` with all values
#'   below one set to one.
#' @keywords internal
set_spaces <- function(spaces_after_prefix, force_one) {
  if (force_one) {
    n_of_spaces <- rep(1, length(spaces_after_prefix))
  } else {
    n_of_spaces <- pmax(spaces_after_prefix, 1L)
  }
  n_of_spaces
}

#' Nest a flat parse table
#'
#' `nest_parse_data` groups `pd_flat` into a parse table with tokens that are
#'  a parent to other tokens (called internal) and such that are not (called
#'  child). Then, the token in child are joined to their parents in internal
#'  and all token information of the children is nested into a column "child".
#'  This is done recursively until we are only left with a nested tibble that
#'  contains one row: The nested parse table.
#' @param pd_flat A flat parse table including both terminals and non-terminals.
#' @seealso [compute_parse_data_nested()]
#' @return A nested parse table.
#' @importFrom purrr map2
#' @keywords internal
nest_parse_data <- function(pd_flat) {
  if (all(pd_flat$parent <= 0)) {
    return(pd_flat)
  }
  pd_flat$internal <- with(pd_flat, (id %in% parent) | (parent <= 0))
  split_data <- split(pd_flat, pd_flat$internal)

  child <- split_data$`FALSE`
  internal <- split_data$`TRUE`

  internal$internal_child <- internal$child
  internal$child <- NULL

  child$parent_ <- child$parent
  joined <-
    child %>%
    nest_(., "child", setdiff(names(.), "parent_")) %>%
    left_join(internal, ., by = c("id" = "parent_"))
  nested <- joined
  nested$child <- map2(nested$child, nested$internal_child, combine_children)
  nested <- nested[, setdiff(names(nested), "internal_child")]
  nest_parse_data(nested)
}

#' Combine child and internal child
#'
#' Binds two parse tables together and arranges them so that the tokens are in
#' the correct order.
#' @param child A parse table or `NULL`.
#' @param internal_child A parse table or `NULL`.
#' @details Essentially, this is a wrapper around [dplyr::bind_rows()], but
#'   returns `NULL` if the result of [dplyr::bind_rows()] is a data frame with
#'   zero rows.
#' @keywords internal
combine_children <- function(child, internal_child) {
  bound <- bind_rows(child, internal_child)
  if (nrow(bound) == 0) {
    return(NULL)
  }
  bound[order(bound$pos_id), ]
}
