### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### style-files                                                             ####

# success
run_test("style-files", suffix = "-success.R")
# fail
run_test("style-files", suffix = "-fail-changed.R", error_msg = NA)

run_test("style-files", suffix = "-fail-parse.R", error_msg = "unexpected")

# success with cmd args
run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-success.R",
  cmd_args = c("--style_pkg=styler", "--style_fun=tidyverse_style")
)

run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-fail.R",
  error_msg = NA,
  cmd_args = c("--style_pkg=styler", "--style_fun=tidyverse_style")
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
run_test("spell-check",
  suffix = "-wordlist-success.md",
  error_msg = NULL,
  copy = c("inst/WORDLIST" = test_path("in/WORDLIST"))
)

# success with ignored files
# uses lang argument
run_test("spell-check", suffix = "-language-success.md", cmd_args = "--lang=en_GB")



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### depds-in-desc                                                           ####
# succeed (call to library that is in description)
run_test("deps-in-desc",
  suffix = "-success.R", error_msg = NULL,
  copy = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)
# fail (call to library that is not in description)
run_test("deps-in-desc",
  suffix = "-fail.R", error_msg = "Dependency check failed",
  copy = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

# with :::
run_test("deps-in-desc",
  "deps-in-desc-dot3",
  suffix = "-fail.R", error_msg = "Dependency check failed",
  copy = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc-dot3",
  suffix = "-success.R", error_msg = NULL,
  copy = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc-dot3",
  suffix = "-fail.R", error_msg = NULL,
  copy = c("DESCRIPTION" = test_path("in/DESCRIPTION")),
  cmd_args = "--allow_private_imports"
)




### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### lintr                                                                   ####

# success
run_test("lintr",
  suffix = "-success.R",
  error_msg = NULL
)

# failure
run_test("lintr", suffix = "-fail.R", error_msg = "not lint free")

# warning
run_test(
  "lintr",
  suffix = "-fail.R", cmd_args = "--warn_only", error_msg = NULL
)
