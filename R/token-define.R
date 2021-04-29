token <- tribble(
  ~text,           ~class,         ~token,
  "&",          "logical",          "AND",
  "&&",         "logical",         "AND2",
  "|",          "logical",           "OR",
  "||",         "logical",          "OR2",
  ">",          "logical",           "GT",
  "<",          "logical",           "LT",
  "<=",         "logical",           "LE",
  ">=",         "logical",           "GE",
  "!=",         "logical",           "NE",
  "==",         "logical",           "EQ",
  "=",      "assign_left",       "EQ_SUB",
  "=",      "assign_left",    "EQ_ASSIGN",
  "<-",     "assign_left",  "LEFT_ASSIGN",
  "->",    "assign_right", "RIGHT_ASSIGN",
  "+",             "math",          "'+'",
  "-",             "math",          "'-'",
  "*",             "math",          "'*'",
  "/",             "math",          "'/'",
  "^",             "math",          "'^'",
  "~",          "formula",          "'~'",
  "if",            "cond",           "IF",
  "else",          "cond",         "ELSE",
  "in",       "loop_cond",           "IN",
  "while",    "loop_cond",        "WHILE"
)

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
#' @importFrom purrr map_chr
#' @keywords internal
lookup_new_special <- function(regex = NA) {
  new_special <- c("PIPE", "IN", "OTHER")

  potential_regex <- grep(regex, new_special, value = TRUE, ignore.case = TRUE)
  if (is.na(regex)) {
    mapping <- new_special
  } else if (length(potential_regex) > 0) {
    mapping <- potential_regex
  } else {
    return(NA)
  }
  map_chr(mapping, special_and)
}

special_token <- lookup_new_special()

op_token <- c(
  special_token,
  logical_token,
  left_assignment_token,
  right_assignment_token,
  "EQ_SUB", "ELSE", "IN",
  "EQ_FORMALS"
)
