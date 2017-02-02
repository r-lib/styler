re_substitutes_global <- function(text, search, replace) {
  text <- rex::re_substitutes(text, search, replace, global = TRUE)
}

re_substitutes_repeat <- function(text, search, replace, global = TRUE) {
  repeat {
    old_text <- text
    text <- rex::re_substitutes(text, search, replace, global = global)
    if (all(text == old_text))
      break
  }
  text
}
