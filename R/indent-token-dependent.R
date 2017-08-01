#' Make indention token-dependent
#'
#' Updates the indention of certain tokens to be dependent on the position
#' of other tokens.
#' @inheritParams set_token_dependent_indention_one
#' @details For details of the implmementation, see
#'   [set_token_dependent_indention_one()].
#' @note
#' See 'Examples' for two examples of how formatted code should look like. Note
#' that the second example is not compliant with the tidyverse style guide.
#' In this case, the approach to take would be to have a transformer function
#' / rule that breaks the line after the opening parenthesis anyways, so no
#' token-dependent indention would be needed. Nevertheless, the case should be
#' covered because other style guides may depend on this reformatting and do
#' not implement a rule that adds a line break after the opening parenthesis.
#' @examples
#' \dontrun{
#' a <- function(x,
#'               y) {
#'   x
#' }
#'
#' call(1,
#'      2)
#' }
set_token_dependent_indention <- function (flattened_pd) {

  parents_to_update <- compute_parent_to_update(flattened_pd)

  for (index in parents_to_update) {
    flattened_pd <- set_token_dependent_indention_one(flattened_pd, index)
  }
  flattened_pd
}

#' Compute the indices of the parents that are to be updated
#' @param flattened_pd A flattened parse table.
compute_parent_to_update <- function(flattened_pd) {
  which(flattened_pd$token == "'('" & flattened_pd$newlines == 0)
}


#' Set the token-dependent indention
#'
#' Set the token-dependent indention for one target index.
#' @param flattened_pd A flattened parse table.
#' @param target_index The index of the token in `flattened_pd` that has the
#'   indention we want to apply to other tokens.
#' @details
#' Updating indention is carried out in two steps: updating the indention itself
#' and updating the position of the corresponding tokens in the colum `col`, so
#' subsequent indention based on re-indented tokens can be done correctly.
#'
#' The idea is to first find the token on which the indention of other tokens
#' should be based on. This is always an opening parenthesis.
#' Then, via `id`, it is possible to identify the closing parenthesis that
#' belongs to the expresion.
#' Since the column `indent` was removed in [enrich_terminals()]
#' and all indention information is now stored in the column `spaces` of the
#' first token of every line, spacing is only updated for the tokens after a
#' line break. `lag_spaces` of these tokens will be updated to match
#' the position of the token right after the opening parenthesis as shown in the
#' example in [set_token_dependent_indention()]
#'
#' All re-indented lines now have oudated `col` values. Hence, these now need to
#' be updated according to the number of spaces that were added
#' to or removed from the first token on the line. This is necessary because
#' subsequent re-indention depends on `col` values.
#' @return A parse table with updated indention for `target_index`.
set_token_dependent_indention_one <- function(flattened_pd, target_index) {

  cols_to_update <- compute_cols_to_update(flattened_pd, target_index)

  if (length(cols_to_update) < 1) return(flattened_pd)
  if (!needs_reindention(flattened_pd, target_index))
    return(flattened_pd)

  spaces_to_update <- compute_spaces_to_update(flattened_pd, cols_to_update)
  if (length(spaces_to_update) < 1) return(flattened_pd)

  shift <- compute_shift_from_col(
    flattened_pd,
    target_index,
    spaces_to_update[1]
  )

  flattened_pd <- apply_shift_to_tokens(
    flattened_pd,
    spaces_to_update,
    cols_to_update,
    shift
  )

  flattened_pd
}

#' Return the indices of the tokens for which `col` needs to be udpated
#'
#' `col` needs to be updated for all tokens that lay on a line which will be
#' re-indented. This is equivalent to be positioned between the opening
#' parenthesis (given by `target_index`) and closing parenthesis (which is to
#' be found from `target_index` via the `id` attribute) in `flattened_pd`.
#' @param flattened_pd A flattened parse table.
#' @param target_index The index of the token in `flattened_pd` that has the
#'   indention we want to apply to other tokens.
#' @seealso compute_spaces_to_update
compute_cols_to_update <- function(flattened_pd, target_index) {
  index_start <- compute_start_from_target(flattened_pd, target_index)
  index_stop <- compute_stop_from_start(flattened_pd, target_index)
  cols_to_update <- which(between(
    seq_len(nrow(flattened_pd)),
    index_start, index_stop)
  )

  cols_to_update
}


#' Compute the start given a target index
#'
#' Computes the index of the first token for which `col` needs to be updated.
#' This is the first token on the line following `target_index`.
compute_start_from_target <- function(flattened_pd, target_index) {
  line_of_start <- flattened_pd$line1[target_index] + 1
  which(flattened_pd$line1 == line_of_start)[1]
}

#' Compute the stop given start
#'
#' Computes the index of the last token for which `col` needs to be updated.
#' If the line where the last token sits starts with
#' a closing parenthesis, the stopping token is the last token on the line
#' before, otherwise it is the last token on the same line as the closing
#' parenthesis.
compute_stop_from_start <- function(flattened_pd, target_index) {
  parent_of_ids_to_update <- flattened_pd$parent[target_index]
  index_closing <- last(which(flattened_pd$parent == parent_of_ids_to_update))
  line_of_stop <- flattened_pd$line1[index_closing]

  first_on_line_stop <- first(which(flattened_pd$line1 == line_of_stop))
  last_on_line_stop <- last(which(flattened_pd$line1 == line_of_stop))
  if (flattened_pd$token[first_on_line_stop] %in% c("'}'", "')'")) {
    first_on_line_stop - 1
  } else {
    last_on_line_stop
  }
}


#' Check whether an expression needs reindention
#'
#' Check whether an opening round brace is on the same line. If not, check
#' whether a curly opening brace is there or whether there is nothing following
#' on the same line at all. If so, there is no need to indent.
#'
#' If there is an opening round brace on the same line, check whether the
#' closing brace is on that very same line too. If so, re-indention is needed,
#' otherwise not.
#' @param flattened_pd A flattened parse table.
#' @param target_index The index of the token in `flattened_pd` that has the
#'   indention we want to apply to other tokens.
#' @param cols_to_update The indices of the tokens in the parse table which
#'   should be shifted.
needs_reindention <- function(flattened_pd, target_index) {

  next_opening_on_same_line <-
    token_on_same_line_as(flattened_pd, target_index, "'('")[1]

  if (is.na(next_opening_on_same_line)) {
    if (line_break_after(flattened_pd, target_index)) {
      return(FALSE)
    } else if (length(token_on_same_line_as(flattened_pd, target_index, "'{'")) > 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  next_closing <- last(which(
    flattened_pd$parent ==
    flattened_pd$parent[next_opening_on_same_line]
  ))

  flattened_pd$line1[next_opening_on_same_line] == flattened_pd$line1[next_closing]


}

#' Check whether a line break follows
#'
#' Check wheter a code line break follows, that is, whether a line break
#' separates `target_index` and the next non-comment element.
line_break_after <- function(flattened_pd, target_index) {
  if (flattened_pd$token[target_index + 1] == "COMMENT") {
    return(TRUE)
  } else {
    flattened_pd$lag_newlines[target_index + 1] > 0
  }
}


#' Return the indices of a token on the line of a target index
#'
#' Returns only tokens that are coming *after* target_index and are on
#' the same line.
token_on_same_line_as <- function(flattened_pd, target_index, token) {
  all <- which(flattened_pd$token == token)
  index <- all > target_index &
    (flattened_pd$line1[all] == flattened_pd$line1[target_index])
  all[index]
}


#' Which spaces need an update?
#'
#' Given indices for which cols needs to be updated, find spaces that need to
#' be updated. Only the tokens in `cols_to_update` that lay on a new line need
#' their `spaces` attribute to be updated. All tokens on the same line don't
#' need extra re-indention, since they are going to be shifted by the same
#' amount as the first token.
#' @param flattened_pd A flattened parse table.
#' @param cols_to_update The indices of the tokens in the parse table which
#'   should be shifted.
#' @seealso compute_cols_to_update
compute_spaces_to_update <- function(flattened_pd, cols_to_update) {
  after_line_break <- which(flattened_pd$lag_newlines > 0)
  intersect(cols_to_update, after_line_break)
}


#' How much should we shift?
#'
#' Compute the amount of spaces needed to be inserted before some tokens to
#' match the target indention.
#' @param flattened_pd A flattened parse table.
#' @param target_index The index of the token in `flattened_pd` that has the
#'   indention we want to apply to other tokens.
#' @param subject_index The index of a token in `flattened_pd` that
#'   should match the target indention after re-indention.
compute_shift_from_col <- function(flattened_pd, target_index, subject_index) {
  subject_col <- flattened_pd$col1[subject_index]
  target_col <- flattened_pd$col1[target_index]
  shift <- target_col - subject_col + 1
  shift
}



#' Shift the position of tokens by a certain amount of spaces
#'
#' Shifts tokens in a flattened parse table. For
#' the indices `spaces_to_update`, this entails increasing both the column
#' `spaces` and `col` by `shift`. For the indices `col_to_update`, only the
#' `col` attribute will be updated since we do not want to add extra spaces
#' between the tokens on the same line.
#' @param spaces_to_update The indices of the first tokens on every line in the
#'   parse table which should be shifted.
#' @param cols_to_update The indices of the tokens in the parse table which
#'   should be shifted.
apply_shift_to_tokens <- function(flattened_pd,
                                  spaces_to_update,
                                  cols_to_update,
                                  shift) {
  # shift the tokens
  flattened_pd$lag_spaces[spaces_to_update] <-
    flattened_pd$lag_spaces[spaces_to_update] + shift

  # update col
  flattened_pd$col1[cols_to_update] <-
    flattened_pd$col1[cols_to_update]  + shift

  flattened_pd$col2[cols_to_update] <-
    flattened_pd$col2[cols_to_update]  + shift

  flattened_pd
}

