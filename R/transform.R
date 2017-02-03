transform_files <- function(files, transformers) {
  transformer <- function(text) {
    new_text <- Reduce(
      function(text, transformer) transformer(text),
      transformers,
      init = text)
    new_text
  }

  changed <- utf8::transform_lines_enc(files, transformer)
  if (any(changed)) {
      message("Please review the changes carefully!")
  }
  invisible(changed)
}
