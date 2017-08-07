add_brackets_in_pipe <- function(pd) {
  has_no_brackets <- (pd$token_before == "SPECIAL-PIPE") &
    (pd$token == "SYMBOL") & (pd$text != ".")
  if (!any(has_no_brackets)) return(pd)
  new <- data_frame(token = c("'('", "')'"),
             text  = c("(", ")"),
             lag_newlines = rep(0, 2),
             terminal = rep(TRUE, 2),
             spaces = rep(0, 2),
             line1 = pd$line2[has_no_brackets] + 1:2,
             line2 = line1,
             col1 = pd$col1[has_no_brackets],
             col2 = col1,
             indent = rep(0, 2),
             child = rep(list(NULL), 2)
         )
  pd <- bind_rows(pd, new)
  pd

}

updat_indention_ref <- function(pd_nested) {
  if ((pd_nested$token_before[2] %in% c("SYMBOL_FUNCTION_CALL")) &&
       nrow(pd_nested) > 3 &&
       pd_nested$lag_newlines[3] == 0) {
    seq <- 3:(nrow(pd_nested) - 1)
    one_line_seqs <- setdiff(seq, which(pd_nested$multi_line))
    pd_nested$indent_ref_id[one_line_seqs] <- pd_nested$child[[1]]$id
  }
  pd_nested
}
