#' @include token.R
add_space_around_op <- function(pd_flat) {
  op_after <- pd_flat$token %in% op_token
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx_before] <- pmax(pd_flat$spaces[idx_before], 1L)
  idx_after <- op_after & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx_after] <- pmax(pd_flat$spaces[idx_after], 1L)
  pd_flat
}

#' @include token.R
set_space_around_op <- function(pd_flat) {
  op_after <- pd_flat$token %in% op_token
  if (!any(op_after)) return(pd_flat)
  op_before <- lead(op_after, default = FALSE)
  pd_flat$spaces[op_before & (pd_flat$newlines == 0L)] <- 1L
  pd_flat$spaces[op_after & (pd_flat$newlines == 0L)] <- 1L
  pd_flat
}

# depreciated!
#' @include token.R
remove_space_after_unary_pm <- function(pd_flat) {
  op_pm <- c("'+'", "'-'")
  op_pm_unary_after <- c(op_pm, op_token, "'('", "','")

  pm_after <- pd_flat$token %in% op_pm
  pd_flat$spaces[pm_after & (pd_flat$newlines == 0L) &
                   (dplyr::lag(pd_flat$token) %in% op_pm_unary_after)] <- 0L
  pd_flat
}


remove_space_after_unary_pm_nested <- function(pd) {
  if (any(pd$token[1] %in% c("'+'", "'-'"))) {
    pd$spaces[1] <- 0L
  }

  pd
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
  if (!any(paren_after)) return(pd_flat)
  paren_before <- lead(paren_after, default = FALSE)
  pd_flat$spaces[paren_before & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

remove_space_after_opening_paren <- function(pd_flat) {
  paren_after <- pd_flat$token == "'('"
  if (!any(paren_after)) return(pd_flat)
  pd_flat$spaces[paren_after & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

remove_space_before_closing_paren <- function(pd_flat) {
  paren_after <- pd_flat$token == "')'"
  if (!any(paren_after)) return(pd_flat)
  paren_before <- lead(paren_after, default = FALSE)
  pd_flat$spaces[paren_before & (pd_flat$newlines == 0L)] <- 0L
  pd_flat
}

add_space_after_for_if_while <- function(pd_flat) {
  comma_after <- pd_flat$token %in% c("FOR", "IF", "WHILE")
  if (!any(comma_after)) return(pd_flat)
  idx <- comma_after & (pd_flat$newlines == 0L)
  pd_flat$spaces[idx] <- pmax(pd_flat$spaces[idx], 1L)
  pd_flat
}

add_space_before_brace <- function(pd_flat) {
  op_after <- pd_flat$token %in% "'{'"
  if (!any(op_after)) return(pd_flat)
  op_before <- lead(op_after, default = FALSE)
  idx_before <- op_before & (pd_flat$newlines == 0L) & pd_flat$token != "'('"
  pd_flat$spaces[idx_before] <- pmax(pd_flat$spaces[idx_before], 1L)
  pd_flat
}

add_space_after_comma <- function(pd_flat) {
  comma_after <- pd_flat$token == "','"
  pd_flat$spaces[comma_after] <- pmax(pd_flat$spaces[comma_after], 1L)
  pd_flat
}

set_space_after_comma <- function(pd_flat) {
  comma_after <- pd_flat$token == "','"
  pd_flat$spaces[comma_after] <- 1L
  pd_flat
}

remove_space_before_comma <- function(pd_flat) {
  comma_after <- (pd_flat$token == "','")
  if (!any(comma_after)) return(pd_flat)
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
  if (pd_flat$token[1] %in% c("FUNCTION", "IF", "WHILE")) {
    index <- pd_flat$token == "')'" & pd_flat$newlines == 0L
    pd_flat$spaces[index] <- 1L
  } else if (pd_flat$token[1] == "FOR") {
    index <- 2
    pd_flat$spaces[index] <- 1L
  }
  pd_flat
}

#' Start comments with a space
#'
#' Forces comments to start with a space, that is, after the regular expression
#'   "^#+'*", at least one space must follow if the comment is *non-empty*, i.e
#'   there is not just spaces within the comment. Multiple spaces may be legit
#'   for indention in some situations.
#' @param pd A parse table.
#' @param force_one Wheter or not to force one space or allow multiple spaces
#'   after the regex "^#+'*".
#' @importFrom purrr map_chr
start_comments_with_space <- function(pd, force_one = FALSE) {
  comment_pos <- pd$token == "COMMENT"
  if (!any(comment_pos)) return(pd)

  comments <- pd[comment_pos, ]

  non_comments <-pd[pd$token != "COMMENT", ]

  comments <-  extract(comments, text,
                       c("prefix", "space_after_prefix", "text"),
                       regex = "^(#+'*)( *)(.*)$")
  comments$space_after_prefix <- nchar(comments$space_after_prefix)
  comments$space_after_prefix <- set_spaces(
    spaces_after_prefix = comments$space_after_prefix,
    force_one
  )

  comments$text <-
    paste0(
      comments$prefix,
      map_chr(comments$space_after_prefix, rep_char, char = " "),
      comments$text
    ) %>%
    trimws("right")
  comments$short <- substr(comments$text, 1, 5)

  comments[, setdiff(names(comments), c("space_after_prefix", "prefix"))] %>%
    bind_rows(non_comments) %>%
    arrange(line1, col1)
}


set_space_before_comments <- function(pd_flat) {
  comment_after <- (pd_flat$token == "COMMENT") & (pd_flat$lag_newlines == 0L)
  if (!any(comment_after)) return(pd_flat)
  comment_before <- lead(comment_after, default = FALSE)
  pd_flat$spaces[comment_before & (pd_flat$newlines == 0L)] <- 1L
  pd_flat

}

remove_space_after_excl <- function(pd_flat) {
  excl <- (pd_flat$token == "'!'") & (pd_flat$newlines == 0L)
  pd_flat$spaces[excl] <- 0L
  pd_flat
}
