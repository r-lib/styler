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
