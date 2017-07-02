op_token <- c(
  "'+'", "'-'", "'*'", "'/'", "'^'", "AND", "AND2", "EQ", "EQ_ASSIGN",
  "GE", "GT", "LE", "LEFT_ASSIGN", "LT", "NE", "OR", "OR2", "RIGHT_ASSIGN",
  "SPECIAL", "EQ_SUB", "ELSE")

add_space_around_op <- function(pd_flat) {
  op_after <- pd_flat$token %in% op_token
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx_before] <- pmax(pd_flat$spaces[idx_before], 1L)
  idx_after <- op_after & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx_after] <- pmax(pd_flat$spaces[idx_after], 1L)
  pd_flat
}

set_space_around_op <- function(pd_flat) {
  op_after <- pd_flat$token %in% op_token
  op_before <- lead(op_after, default = FALSE)
  pd_flat$spaces[op_before & (pd_flat$newlines == 0L)] <- 1L
  pd_flat$spaces[op_after & (pd_flat$newlines == 0L)] <- 1L
  pd_flat
}

remove_space_after_unary_pm <- function(pd_flat) {
  op_pm <- c("'+'", "'-'")
  op_pm_unary_after <- c(op_pm, op_token, "'('", "','")

  pm_after <- pd_flat$token %in% op_pm
  pd_flat$spaces[pm_after & (pd_flat$newlines == 0L) &
                   dplyr::lag(pd_flat$token) %in% op_pm_unary_after] <- 0L
  pd_flat
}

fix_quotes <- function(pd_flat) {
  str_const <- pd_flat$token == "STR_CONST"
  str_const_change <- grepl("^'([^\"]*)'$", pd_flat$text[str_const])
  pd_flat$text[str_const][str_const_change] <-
    vapply(
      lapply(pd_flat$text[str_const][str_const_change], parse_text),
      deparse,
      character(1L))
  pd_flat
}

remove_space_before_opening_paren <- function(pd_flat) {
  paren_after <- pd_flat$token == "'('"
  paren_before <- lead(paren_after, default = FALSE)
  pd_flat$spaces[paren_before & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

remove_space_after_opening_paren <- function(pd_flat) {
  paren_after <- pd_flat$token == "'('"
  pd_flat$spaces[paren_after & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

remove_space_before_closing_paren <- function(pd_flat) {
  paren_after <- pd_flat$token == "')'"
  paren_before <- lead(paren_after, default = FALSE)
  pd_flat$spaces[paren_before & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

add_space_after_for_if_while <- function(pd_flat) {
  comma_after <- pd_flat$token %in% c("FOR", "IF", "WHILE")
  idx <- comma_after & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx] <- pmax(pd_flat$spaces[idx], 1L)
  pd_flat
}

add_space_before_brace <- function(pd_flat) {
  op_after <- pd_flat$token %in% "'{'"
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd_flat$newlines == 0L) & pd_flat$token != "'('"
  pd_flat$spaces[idx_before] <- pmax(pd_flat$spaces[idx_before], 1L)
  pd_flat
}

add_space_after_comma <- function(pd_flat) {
  comma_after <- pd_flat$token == "','"
  idx <- comma_after & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx] <- pmax(pd_flat$spaces[idx], 1L)
  pd_flat
}

set_space_after_comma <- function(pd_flat) {
  comma_after <- pd_flat$token == "','"
  idx <- comma_after & (pd_flat$newlines == 0L)
  pd_flat$spaces[comma_after & (pd_flat$newlines == 0L)] <- 1L
  pd_flat
}

remove_space_before_comma <- function(pd_flat) {
  comma_after <- pd_flat$token == "','"
  comma_before <- lead(comma_after, default = FALSE)
  idx <- comma_before  & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx] <- 0L
  pd_flat
}


#' Set space between levels of nesting
#'
#' With the nested approach, certain rules do not have an effect anymore because
#'   of the nature of the nested structure. Setting spacing before curly
#'   brackets in for / if / while statements and function declarations will be
#'   such a case since a curly bracket is always at the first position in a
#'   parse table, so spacing cannot be set after the previous token.
#' @param pd_flat A flat parse table.
set_space_between_levels <- function(pd_flat) {
  if (pd_flat$token[1] %in% c("FUNCTION", "FOR", "IF", "WHILE")) {
    pd_flat$spaces[nrow(pd_flat) - 1] <- 1L
  }
  pd_flat
}

#' Start comments with a space
#'
#' Forces comments to start with a space, that is, after the regular expression
#'   "^#+'*", at least one space must follow. Multiple spaces may be legit for
#'   indention in some situations.
#'
#' @param pd A parse table.
#' @param force_one Wheter or not to force one space or allow multiple spaces
#'   after the regex "^#+'*".
start_comments_with_space <- function(pd, force_one = FALSE) {
  comments <- pd %>%
    filter(token == "COMMENT")

  if (nrow(comments) == 0) return(pd)

  non_comments <- pd %>%
    filter(token != "COMMENT")

  comments <- comments %>%
    extract(text, c("prefix", "space_after_prefix", "text"),
            regex = "^(#+'*)( *)(.*)$") %>%
    mutate(space_after_prefix = nchar(space_after_prefix),
           space_after_prefix = set_spaces(space_after_prefix, force_one),
           text = paste0(prefix, rep_char(" ", space_after_prefix), text),
           short = substr(text, 1, 5)) %>%
    select(-space_after_prefix, -prefix)
  bind_rows(comments, non_comments) %>%
    arrange(line1, col1)
}


set_space_before_comments <- function(pd_flat) {
  comment_after <- pd_flat$token == "COMMENT"
  comment_before <- lead(comment_after, default = FALSE)
  pd_flat$spaces[comment_before & (pd_flat$newlines == 0L)] <- 1L
  pd_flat

}
