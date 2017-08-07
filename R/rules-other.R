add_brackets_in_pipe <- function(pd) {
  has_no_brackets <- (pd$token_before == "SPECIAL-PIPE") &
    (pd$token == "SYMBOL") & (pd$text != ".")
  if (!any(has_no_brackets)) return(pd)
  new <- data_frame(token = c("'('", "')'"),
             text  = c("(", ")"),
             lag_newlines = rep(0, 2),
             terminal = rep(TRUE, 2),
             spaces = rep(0, 2),
             line1 = pd$line1[has_no_brackets],
             line2 = line1,
             col1 = pd$col1[has_no_brackets],
             col2 = col1 + 1:2,
             indent = rep(0, 2),
             child = rep(list(NULL), 2)
         )
  pd <- bind_rows(pd, new)
  pd

}
