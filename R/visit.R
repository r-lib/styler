#' Visit'em all
#'
#' Apply a list of functions to each level in a nested parse table.
#' `pre_visit()` applies `funs` before it proceeds to the children,
#' (that is, starts from the outermost level of nesting progressing
#' to the innermost level), `post_visit()` proceeds to its children
#' before applying the functions (meaning it first applies the functions
#' to the innermost level of nesting first and then going outwards).
#' @param pd_nested A nested parse table.
#' @inheritParams visit_one
#' @family visitors
#' @name visit
#' @keywords internal
NULL

#' @rdname visit
#' @keywords internal
pre_visit <- function(pd_nested, funs) {
  if (is.null(pd_nested)) {
    return()
  }
  if (length(funs) == 0L) {
    return(pd_nested)
  }
  pd_nested <- visit_one(pd_nested, funs)

  children <- pd_nested$child
  for (i in seq_along(children)) {
    child <- children[[i]]
    if (!is.null(child)) {
      children[[i]] <- pre_visit(child, funs)
    }
  }
  pd_nested$child <- children
  pd_nested
}

#' @rdname visit
#' @keywords internal
pre_visit_one <- function(pd_nested, fun) {
  if (is.null(pd_nested)) {
    return()
  }
  pd_nested <- fun(pd_nested)

  children <- pd_nested$child
  for (i in seq_along(children)) {
    child <- children[[i]]
    if (!is.null(child)) {
      children[[i]] <- pre_visit_one(child, fun)
    }
  }
  pd_nested$child <- children
  pd_nested
}

#' @rdname visit
#' @keywords internal
post_visit <- function(pd_nested, funs) {
  if (is.null(pd_nested)) {
    return()
  }
  if (length(funs) == 0L) {
    return(pd_nested)
  }

  children <- pd_nested$child
  for (i in seq_along(children)) {
    child <- children[[i]]
    if (!is.null(child)) {
      children[[i]] <- post_visit(child, funs)
    }
  }
  pd_nested$child <- children

  visit_one(pd_nested, funs)
}

#' @rdname visit
#' @keywords internal
post_visit_one <- function(pd_nested, fun) {
  if (is.null(pd_nested)) {
    return()
  }
  force(fun)

  children <- pd_nested$child
  for (i in seq_along(children)) {
    child <- children[[i]]
    if (!is.null(child)) {
      children[[i]] <- post_visit_one(child, fun)
    }
  }
  pd_nested$child <- children

  fun(pd_nested)
}

#' Transform a flat parse table with a list of transformers
#'
#' Uses [Reduce()] to apply each function of `funs` sequentially to
#'   `pd_flat`.
#' @param pd_flat A flat parse table.
#' @param funs A list of transformer functions.
#' @family visitors
#' @keywords internal
visit_one <- function(pd_flat, funs) {
  for (f in funs) {
    pd_flat <- f(pd_flat)
  }
  pd_flat
}

#' Propagate context to terminals
#'
#' Implements a very specific pre-visiting scheme, namely to propagate
#' indention, spaces and lag_newlines to inner token to terminals. This means
#' that information regarding indention, line breaks and spaces (which is
#' relative in `pd_nested`) will be converted into absolute.
#' @inherit context_towards_terminals
#' @seealso context_towards_terminals visitors
#' @keywords internal
context_to_terminals <- function(pd_nested,
                                 outer_lag_newlines,
                                 outer_indent,
                                 outer_spaces,
                                 outer_indention_refs) {
  if (is.null(pd_nested)) {
    return()
  }

  pd_transformed <- context_towards_terminals(
    pd_nested,
    outer_lag_newlines, outer_indent,
    outer_spaces, outer_indention_refs
  )

  pd_transformed$child <- pmap(
    list(
      pd_transformed$child,
      pd_transformed$lag_newlines,
      pd_transformed$indent,
      pd_transformed$spaces,
      pd_transformed$indention_ref_pos_id
    ),
    context_to_terminals
  )
  pd_transformed
}

#' Update the a parse table given outer context
#'
#' `outer_lag_newlines` are added to the first token in `pd`,
#' `outer_indent` is added to all tokens in `pd`, `outer_spaces` is added to the
#' last token in `pd`. [context_to_terminals()] calls this function repeatedly,
#' which means the propagation of the parse information to the terminal tokens.
#' @param pd_nested A nested parse table.
#' @param outer_lag_newlines The lag_newlines to be propagated inwards.
#' @param outer_indent The indention depth to be propagated inwards.
#' @param outer_spaces The number of spaces to be propagated inwards.
#' @param outer_indention_refs The reference pos id that should be propagated
#'   inwards.
#' @return An updated parse table.
#' @seealso context_to_terminals
#' @keywords internal
context_towards_terminals <- function(pd_nested,
                                      outer_lag_newlines,
                                      outer_indent,
                                      outer_spaces,
                                      outer_indention_refs) {
  pd_nested$indent <- pd_nested$indent + ifelse(
    is.na(pd_nested$indention_ref_pos_id),
    outer_indent,
    0L
  )
  ref_pos_id_is_na <- !is.na(pd_nested$indention_ref_pos_id)
  pd_nested$indention_ref_pos_id[!ref_pos_id_is_na] <- outer_indention_refs
  pd_nested$lag_newlines[1L] <- pd_nested$lag_newlines[1L] + outer_lag_newlines
  pd_nested$spaces[nrow(pd_nested)] <-
    pd_nested$spaces[nrow(pd_nested)] + outer_spaces
  pd_nested
}

#' Extract terminal tokens
#'
#' Turns a nested parse table into a flat parse table and extracts *all*
#' attributes.
#' @param pd_nested A nested parse table.
#' @keywords internal
extract_terminals <- function(pd_nested) {
  terminal <- pd_nested$terminal
  is_cached <- pd_nested$is_cached

  child <- pd_nested$child

  for (i in seq_len(nrow(pd_nested))) {
    if (terminal[[i]] || is_cached[[i]]) {
      child[[i]] <- list(vec_slice(pd_nested, i))
    }
  }

  # child is a list of data frame lists here
  unlist(unname(child), recursive = FALSE)
}

#' Enrich flattened parse table
#'
#' Enriches a flattened parse table with terminals only. In particular, it is
#' possible to compute the exact position a token will have (line and column)
#' when it will be serialized.
#' @details
#' Since we have only terminal tokens now, the line on which a token starts we
#' also be the line on which it ends. We call `line1` the line on which the
#' token starts. `line1` has the same meaning as `line1` that can be found in a
#' flat parse table (see [tokenize()]), just that the `line1` created by
#' `enrich_terminals()` is the updated version of the former `line1`. The same
#' applies for `col1` and `col2`. Note that this function does remove the
#' columns `indent` and `spaces.` All information of the former is stored in
#' `lag_spaces` now. The later was removed because it is redundant after adding
#' the column `lag_spaces`, which is more convenient to work with, in particular
#' when serializing the parse table.
#' @inheritParams choose_indention
#' @keywords internal
enrich_terminals <- function(flattened_pd, use_raw_indention = FALSE) {
  flattened_pd$lag_spaces <- lag(flattened_pd$spaces, default = 0L)
  flattened_pd$spaces <- NULL # depreciate spaces
  flattened_pd <- choose_indention(flattened_pd, use_raw_indention)
  flattened_pd$line1 <-
    cumsum(flattened_pd$lag_newlines)

  flattened_pd$newlines <- lead(flattened_pd$lag_newlines, default = 0L)
  flattened_pd$nchar <- nchar(flattened_pd$text, type = "width")
  groups <- flattened_pd$line1
  split_pd <- vec_split(flattened_pd, groups)[[2L]]
  flattened_pd <- split_pd %>%
    purrr::map(function(.x) {
      .x$col2 <- cumsum(.x$nchar + .x$lag_spaces)
      .x
    }) %>%
    purrr::list_rbind()
  flattened_pd$col1 <- flattened_pd$col2 - flattened_pd$nchar
  flattened_pd
}

#' Choose the indention method for the tokens
#'
#' Either use the raw indention, which is just the spaces computed between
#' the first token on a new line and the token before it, or use the indention
#' computed according to the transformer used, which is stored in the column
#' `indention`.
#' All indention information will be combined with the space information for
#' the first token on a new line.
#' If `use_raw_indention` is set, information in the column `indention` will
#' be discarded anyways. If it is not set, the first token on a new line will
#' "inherit" the indention of the whole line.
#' The column `indention` will be removed since all information necessary is
#' contained in the spacing information of the first token on a new line and
#' the position of the tokens will not be changed anymore at this stage.
#' @param flattened_pd A nested parse table that was turned into a flat parse
#'   table using [extract_terminals()].
#' @param use_raw_indention Boolean indicating whether or not the raw indention
#'   should be used.
#' @keywords internal
choose_indention <- function(flattened_pd, use_raw_indention) {
  if (!use_raw_indention) {
    flattened_pd$lag_spaces <- ifelse(flattened_pd$lag_newlines > 0L,
      flattened_pd$indent,
      flattened_pd$lag_spaces
    )
  }
  flattened_pd$indent <- NULL
  flattened_pd
}
