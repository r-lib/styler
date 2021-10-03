
if (file.exists("README.Rmd") & file.exists("README.md")) {
  if (file.info("README.md")$mtime < file.info("README.Rmd")$mtime) {
    rlang::abort("README.md is out of date; please re-knit README.Rmd.")
  }

  file_names_staged <- system2(
    "git", c("diff --cached --name-only"),
    stdout = TRUE
  )
  num_readmes <- length(grepl("^README\\.[R]?md$", file_names_staged))

  if (num_readmes == 1) {
    rlang::abort("README.Rmd and README.md should be both staged.")
  }
}
