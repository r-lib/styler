#' Visit'em all
#'
#' Apply a list of functions to each level in a nested parse table.
#'   `pre_visit()` applies `funs` before it preceeds to the children,
#'   (that is, starts from the outermost level of nesting progressing
#'   to the innermost level), `post_visit()` preceeds to its children
#'   before applying the functions (meaning it first applies the functions
#'   to the innermost level of nesting first and then going outwards).
#' @param pd_nested A nested parse table.
#' @inheritParams visit_one
#' @family visitors
#' @importFrom purrr map
#' @name visit
NULL

#' @rdname visit
pre_visit <- function(pd_nested, funs) {
  if (is.null(pd_nested)) return()
  pd_transformed <- visit_one(pd_nested, funs)

  pd_transformed$child <- map(pd_transformed$child, pre_visit, funs = funs)
  pd_transformed
}

#' @rdname visit
post_visit <- function(pd_nested, funs) {
  if (is.null(pd_nested)) return()
  pd_transformed <- pd_nested

  pd_transformed$child <- map(pd_transformed$child, post_visit, funs = funs)
  visit_one(pd_transformed, funs)
}

#' Transform a flat parse table with a list of transformers
#'
#' Uses [purrr::reduce()] to apply each function of `funs` sequentially to
#'   `pd_flat`.
#' @param pd_flat A flat parse table.
#' @param funs A list of transformer functions.
#' @family visitors
#' @importFrom purrr reduce
visit_one <- function(pd_flat, funs) {
  reduce(funs, function(x, fun) fun(x),
         .init = pd_flat)
}


#' Propagate context to terminals
#'
#' Implements a very specific pre-visiting scheme, namely to propagate
#'   indention, spaces and lag_newlines to inner token to terminals. This means
#'   that information regarding indention, linebreaks and spaces (which is
#'   relative in `pd_nested`) will be converted into absolute.
#' @inherit context_towards_terminals
#' @seealso context_towards_terminals visitors
context_to_terminals <- function(pd_nested,
                                 passed_lag_newlines,
                                 passed_indent,
                                 passed_spaces) {

  if (is.null(pd_nested)) return()

  pd_transformed <- context_towards_terminals(
    pd_nested, passed_lag_newlines, passed_indent, passed_spaces
  )

  pd_transformed$child <- pmap(list(pd_transformed$child,
                                    pd_transformed$lag_newlines,
                                    pd_transformed$indent,
                                    pd_transformed$spaces),
                               context_to_terminals)
  pd_transformed
}


#' Update the a parse table given outer context
#'
#' `passed_lag_newlines` are added to the first token in `pd`,
#'   `passed_indent` is added to all tokens in `pd`, `passed_spaces` is added to
#'   the last token in `pd`. [context_to_terminals()] calls this function
#'   repeatedly, which means the propagation of the parse information to the
#'   terminal tokens.
#' @param pd_nested A nested parse table.
#' @param passed_lag_newlines The lag_newlines to be propagated inwards.
#' @param passed_indent The indention depth to be propagated inwards.
#' @param passed_spaces The number of spaces to be propagated inwards.
#' @return An updated parse table.
#' @seealso context_to_terminals
context_towards_terminals <- function(pd_nested,
                                      passed_lag_newlines,
                                      passed_indent,
                                      passed_spaces) {
  pd_nested$indent <- pd_nested$indent + passed_indent
  pd_nested$lag_newlines[1] <- pd_nested$lag_newlines[1] + passed_lag_newlines
  pd_nested$spaces[nrow(pd_nested)] <-
    pd_nested$spaces[nrow(pd_nested)] + passed_spaces
  pd_nested
}

#' Extract terminal tokens
#'
#' Turns a nested parse table into a flat parse table. In particular it extracts
#'   terminal tokens and the following attributes:
#'
#'  * lag_newlines
#'  * indent
#'  * token
#'  * text
#'  * spaces
#'  * id
#'  * parent
#'  * line1
#' @inheritParams extract_terminals_helper
#' @importFrom readr type_convert col_integer cols
extract_terminals <- function(pd_nested) {
  flat_vec <- extract_terminals_helper(pd_nested) %>%
    unlist()
  nms <- list(
    NULL,
    c("lag_newlines", "indent", "token", "text", "spaces", "id", "parent", "line1")
  )
  flat_tbl <- matrix(flat_vec, ncol = length(nms[[2]]), byrow = TRUE, dimnames = nms) %>%
    as_tibble() %>%
    type_convert(
      col_types = cols(
        lag_newlines = col_integer(),
        indent       = col_integer(),
        spaces       = col_integer()
      )
    )
}

#' Helper to extract terminals
#'
#' @param pd_nested A nested parse table.
extract_terminals_helper <- function(pd_nested) {
  if (is.null(pd_nested)) return(pd)
  pmap(list(pd_nested$terminal, pd_nested$token, pd_nested$text,
            pd_nested$lag_newlines, pd_nested$spaces, pd_nested$indent,
            pd_nested$id, pd_nested$parent, pd_nested$line1, pd_nested$child),
       function(terminal, token, text, lag_newlines, spaces, indent, id,
                parent, line1, child) {
         if (terminal) {
           c(lag_newlines, indent, token, text, spaces, id, parent, line1)
         } else {
           extract_terminals_helper(child)
         }
       })
}

#' Enrich flattened parse table
#'
#' Enriches a flattened parse table with terminals only. In particular, it is
#'   possible to compute the exact position a token will have (line and column)
#'   when it will be serialized.
#' @details Since we have only terminal tokens now, the line on which a token
#'  starts we also be the line on which it ends. We call `line1` the line on
#'  which the token starts. `line1` has the same meaning as `line1` that can be
#'  found in a flat parse table (see [tokenize()]), just that the `line1`
#'  created by `enrich_terminals()` is the updated version of the former
#'  `line1`. The same applies for `col1` and `col2`.
#' @inheritParams choose_indention
enrich_terminals <- function(flattened_pd, use_raw_indention = FALSE) {
  flattened_pd$lag_spaces <- lag(flattened_pd$spaces, default = 0)
  flattened_pd <- choose_indention(flattened_pd, use_raw_indention)
  flattened_pd$line1 <-
    cumsum(flattened_pd$lag_newlines) + flattened_pd$line1[1]

  flattened_pd$newlines <- lead(flattened_pd$lag_newlines, default = 0)
  flattened_pd$nchar <- nchar(flattened_pd$text)
  flattened_pd <- flattened_pd %>%
    group_by(line1) %>%
    mutate(col2 = cumsum(nchar + lag_spaces)) %>%
    ungroup()
  flattened_pd$col1 <- flattened_pd$col2 - flattened_pd$nchar
  flattened_pd
}

#' Choose the indention method for the tokens
#'
#' Either use the raw indention, which is just the spaces computed between
#'   the first token on a new line and the token before it, or use the indention
#'   computed according to the transformer used, which is stored in the column
#'   `indention`.
#'
#'  All indention information will be combined with the space information for
#'  the first token on a new line.
#'  If `use_raw_indention` is set, information in the column `indention` will
#'  be discarded anyways. If it is not set, the first token on a new line will
#'  "inherit" the indention of the whole line.
#'  The column `indention` will be removed since all information necessary is
#'  containted in the spacing information of the first token on a new line and
#'  the position of the tokens will not be changed anymore at this stage.
#' @param flattened_pd A nested parse table that was turned into a flat parse
#'   table using [extract_terminals()].
#' @param use_raw_indention Boolean indicating wheter or not the raw indention
#'   should be used.
choose_indention <- function(flattened_pd, use_raw_indention) {
  if (!use_raw_indention) {
    flattened_pd$lag_spaces <- ifelse(flattened_pd$lag_newlines > 0,
                                      flattened_pd$indent,
                                      flattened_pd$lag_spaces)
  }
  flattened_pd$indent <- NULL
  flattened_pd
}


