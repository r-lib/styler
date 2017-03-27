op_token <- c(
  "'+'", "'-'", "'*'", "'/'", "'^'", "AND", "AND2", "EQ", "EQ_ASSIGN",
  "GE", "GT", "LE", "LEFT_ASSIGN", "LT", "NE", "OR", "OR2", "RIGHT_ASSIGN",
  "SPECIAL", "EQ_SUB", "ELSE")

add_space_around_op <- function(pd) {
  op_after <- pd$token %in% op_token
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd$newlines == 0L)
  pd$spaces[idx_before] <- pmax(pd$spaces[idx_before], 1L)
  idx_after <- op_after & (pd$newlines == 0L)
  pd$spaces[idx_after] <- pmax(pd$spaces[idx_after], 1L)
  pd
}

set_space_around_op <- function(pd) {
  op_after <- pd$token %in% op_token
  op_before <- lead(op_after, default = FALSE)
  pd$spaces[op_before & (pd$newlines == 0L)] <- 1L
  pd$spaces[op_after & (pd$newlines == 0L)] <- 1L
  pd
}

remove_space_after_unary_pm <- function(pd) {
  op_pm <- c("'+'", "'-'")
  op_pm_unary_after <- c(op_pm, op_token, "'('", "','")

  pm_after <- pd$token %in% op_pm
  pd$spaces[pm_after & (pd$newlines == 0L) & dplyr::lag(pd$token) %in% op_pm_unary_after] <- 0L
  pd
}

fix_quotes <- function(pd) {
  str_const <- pd$token == "STR_CONST"
  str_const_change <- grepl("^'([^\"]*)'$", pd$text[str_const])
  pd$text[str_const][str_const_change] <-
    vapply(
      lapply(pd$text[str_const][str_const_change], parse_text),
      deparse,
      character(1L))
  pd
}

remove_space_before_opening_paren <- function(pd) {
  paren_after <- pd$token == "'('"
  paren_before <- lead(paren_after, default = FALSE)
  pd$spaces[paren_before & (pd$newlines == 0L)] <- 0L
  pd
}

remove_space_after_opening_paren <- function(pd) {
  paren_after <- pd$token == "'('"
  pd$spaces[paren_after & (pd$newlines == 0L)] <- 0L
  pd
}

remove_space_before_closing_paren <- function(pd) {
  paren_after <- pd$token == "')'"
  paren_before <- lead(paren_after, default = FALSE)
  pd$spaces[paren_before & (pd$newlines == 0L)] <- 0L
  pd
}

add_space_after_for_if_while <- function(pd) {
  comma_after <- pd$token %in% c("FOR", "IF", "WHILE")
  idx <- comma_after & (pd$newlines == 0L)
  pd$spaces[idx] <- pmax(pd$spaces[idx], 1L)
  pd
}

add_space_before_brace <- function(pd) {
  op_after <- pd$token %in% "'{'"
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd$newlines == 0L) & pd$token != "'('"
  pd$spaces[idx_before] <- pmax(pd$spaces[idx_before], 1L)
  pd
}

add_space_after_comma <- function(pd) {
  comma_after <- pd$token == "','"
  idx <- comma_after & (pd$newlines == 0L)
  pd$spaces[idx] <- pmax(pd$spaces[idx], 1L)
  pd
}

set_space_after_comma <- function(pd) {
  comma_after <- pd$token == "','"
  idx <- comma_after & (pd$newlines == 0L)
  pd$spaces[comma_after & (pd$newlines == 0L)] <- 1L
  pd
}
