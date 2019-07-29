source(here::here("R", "infra.R"))


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### style-files                                                             ####

# success
run_test("style-files", suffix = "-success.R")

# fail
run_test("style-files", suffix = "-fail.R", error_msg = NA)

# success with cmd args
run_test("style-files",
  file_name = "style-files-one-line",
  suffix = "-success.R",
  cmd_args = c("--style_pkg=oneliner", "--style_fun=one_line_style")
)

run_test("style-files",
  file_name = "style-files-one-line",
  suffix = "-fail.R",
  error_msg = NA,
  cmd_args = c("--style_pkg=oneliner", "--style_fun=one_line_style")
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### use-tidy-description                                                    ####

# success
run_test("use-tidy-description", "DESCRIPTION", suffix = "")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### no-browser-statement                                                    ####
# success
run_test(
  "no-browser-statement",
  suffix = "-success.R",
  error_msg = NULL
)

# failure
run_test(
  "no-browser-statement",
  suffix = "-fail.R",
  error_msg = "contains a `browser()` statement."
)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### parsable-R                                                              ####

# success
run_test("parsable-R",
  suffix = "-success.R",
  error_msg = NULL
)

# failure
run_test("parsable-R", suffix = "-fail.R", error_msg = "not parsable")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### spell-check                                                             ####
# success
run_test("spell-check", suffix = "-success.md", error_msg = NULL)

# basic failure
run_test("spell-check", suffix = "-fail.md", error_msg = "Spell check failed")

# success with wordlist
test_temp <- function() {
  path_wordlist <- "inst/WORDLIST"
  on.exit(fs::file_delete(path_wordlist))
  fs::dir_create("inst")
  fs::file_create(path_wordlist)
  writeLines("fsssile", path_wordlist)

  run_test("spell-check",
    suffix = "-wordlist-success.md",
    error_msg = NULL,
    copy = "inst/WORDLIST"
  )
}
test_temp()
