ps(
  interaction_constraints = p_uty(tgs = "train"),
  monotone_constraints    = p_uty(dfault = 0, tags = c("train", "control"), custom_check = function(x) {  checkmate::check_integerish(x, lower = -1, upper = 1, any.missing = FALSE) }), # styler: off
  normalize_type          = p_fct(c("tee", "forest"), default = "tree", tags = "train"),
)
