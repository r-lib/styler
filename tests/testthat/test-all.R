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
  tempdir <- tempdir()
  path_inst <- fs::path(tempdir, "inst")
  path_wordlist <- fs::path(path_inst, "WORDLIST")
  fs::dir_create(path_inst)
  fs::file_create(path_wordlist)
  writeLines(c("fsssile", ""), path_wordlist)
  on.exit(fs::file_delete(path_wordlist))

  run_test("spell-check",
    suffix = "-wordlist-success.md",
    error_msg = NULL,
    copy = path_wordlist
  )
}
test_temp()

# success with ignored files
# basic failure
run_test("spell-check", suffix = "-ignored-success.md", cmd_args = "--ignore_files='\\.md$'")

# uses lang argument
run_test("spell-check", suffix = "-language-success.md", cmd_args = "--lang=en_GB")



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### depds-in-desc                                                           ####
test_temp <- function(suffix, error_msg) {
  path_desc <- testthat::test_path("reference-objects/DESCRIPTION")
  path_desc_temp <- fs::path(tempdir(), fs::path_file(path_desc))
  fs::file_copy(path_desc, tempdir(), overwrite = TRUE)
  new_desc <- desc::description$new(path_desc_temp)
  new_desc$set_dep("bliblablupp")
  new_desc$write()
  on.exit(fs::file_delete(path_desc_temp))
  run_test("deps-in-desc",
    suffix = suffix,
    error_msg = error_msg,
    copy = path_desc_temp
  )
}

# succeed (call to library that is in description)
test_temp(suffix = "-success.R", error_msg = NULL)

# fail (call to library that is not in description)
test_temp(suffix = "-fail.R", error_msg = "Dependency check failed")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### lintr                                                                   ####

# success
run_test("lintr",
  suffix = "-success.R",
  error_msg = NULL
)

# failure
run_test("lintr", suffix = "-fail.R", error_msg = "not lint free")
