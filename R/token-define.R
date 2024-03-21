# styler: off
token <- rbind.data.frame(
  c("&",          "logical",          "AND"),
  c("&&",         "logical",         "AND2"),
  c("|",          "logical",           "OR"),
  c("||",         "logical",          "OR2"),
  c(">",          "logical",           "GT"),
  c("<",          "logical",           "LT"),
  c("<=",         "logical",           "LE"),
  c(">=",         "logical",           "GE"),
  c("!=",         "logical",           "NE"),
  c("==",         "logical",           "EQ"),
  c("=",      "assign_left",       "EQ_SUB"),
  c("=",      "assign_left",    "EQ_ASSIGN"),
  c("<-",     "assign_left",  "LEFT_ASSIGN"),
  c("->",    "assign_right", "RIGHT_ASSIGN"),
  c("+",             "math",          "'+'"),
  c("-",             "math",          "'-'"),
  c("*",             "math",          "'*'"),
  c("/",             "math",          "'/'"),
  c("^",             "math",          "'^'"),
  c("~",          "formula",          "'~'"),
  c("if",            "cond",           "IF"),
  c("else",          "cond",         "ELSE"),
  c("in",       "loop_cond",           "IN"),
  c("while",    "loop_cond",        "WHILE"),
  stringsAsFactors = FALSE
)
# styler: on

colnames(token) <- c("text", "class", "token")
math_token <- token$token[token$class == "math"]
logical_token <- token$token[token$class == "logical"]
left_assignment_token <- token$token[token$class == "assign_left"]
right_assignment_token <- token$token[token$class == "assign_right"]

#' Lookup all tokens that have a unique token-text mapping
#' @keywords internal
lookup_tokens <- function() {
  token
}

#' Lookup which new tokens were created from "SPECIAL"
#'
#' @param regex A regular expression pattern to search for.
#' @keywords internal
lookup_new_special <- function(regex = NA) {
  new_special <- c("PIPE", "IN", "OTHER")

  potential_regex <- grep(regex, new_special, value = TRUE, ignore.case = TRUE)
  if (is.na(regex)) {
    mapping <- new_special
  } else if (length(potential_regex) > 0L) {
    mapping <- potential_regex
  } else {
    return(NA)
  }
  map_chr(mapping, special_and)
}

special_token <- lookup_new_special()

op_token <- c(
  special_token,
  "PIPE",
  logical_token,
  left_assignment_token,
  right_assignment_token,
  "EQ_SUB", "ELSE", "IN",
  "EQ_FORMALS"
)

subset_token_opening <- c("'['", "LBB")
