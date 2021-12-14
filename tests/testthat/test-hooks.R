### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### use-tidy-description                                                    ####

# success
run_test("use-tidy-description", "DESCRIPTION", suffix = "")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### style-files                                                             ####

# success
run_test("style-files",
  suffix = "-success.R", cmd_args = c("--cache-root=styler")
)
# fail
run_test("style-files",
  suffix = "-fail-changed.R", cmd_args = c("--cache-root=styler"),
  error_msg = NA
)

run_test("style-files",
  suffix = "-fail-parse.R", cmd_args = c("--cache-root=styler"),
  error_msg = "unexpected"
)

# success with cmd args
run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-success.R",
  cmd_args = c("--style_pkg=styler", "--style_fun=tidyverse_style", "--cache-root=styler")
)

run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-success.R",
  cmd_args = c("--scope=spaces", "--cache-root=styler")
)

run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-success.R",
  cmd_args = c('--scope="I(\'spaces\')"', "--cache-root=styler")
)

run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-success.R",
  cmd_args = c(
    '--scope="I(\'spaces\')"',
    "--base_indention=0",
    "--include_roxygen_examples=TRUE",
    "--cache-root=styler"
  )
)

run_test("style-files",
  file_name = "style-files-reindention",
  suffix = "-success.R",
  cmd_args = c(
    '--scope="I(\'spaces\')"',
    "--base_indention=0",
    "--include_roxygen_examples=TRUE",
    '--reindention="specify_reindention(\'#\')"',
    "--cache-root=styler"
  )
)

run_test("style-files",
  file_name = "style-files",
  suffix = "-base-indention-success.R",
  cmd_args = c("--base_indention=4", "--cache-root=styler")
)

run_test("style-files",
  file_name = "style-files",
  suffix = "-roxygen-success.R",
  cmd_args = c("--include_roxygen_examples=FALSE", "--cache-root=styler")
)

# fail with cmd args
run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-success.R",
  error_msg = "scope must be one",
  cmd_args = c("--scope=space", "--cache-root=styler")
)

run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-fail.R",
  error_msg = NA,
  cmd_args = c(
    "--style_pkg=styler", "--style_fun=tidyverse_style", "--cache-root=styler"
  )
)

run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-fail.R",
  error_msg = "must be listed in `additional_dependencies:`",
  cmd_args = c(
    "--style_pkg=blubliblax", "--style_fun=tidyverse_style", "--cache-root=styler"
  )
)


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

run_test("parsable-R",
  suffix = "-success.Rmd",
  error_msg = NULL
)

# failure
run_test("parsable-R", suffix = "-fail.R", error_msg = "not parsable")

run_test(
  "parsable-R",
  suffix = "-fail.Rmd",
  error_msg = "parsable-R-fail.Rmd is not parsable"
)

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
  artifacts = c("inst/WORDLIST" = test_path("in/WORDLIST"))
)

# success with ignored files
# uses lang argument
run_test("spell-check", suffix = "-language-success.md", cmd_args = "--lang=en_GB")



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### depds-in-desc                                                           ####
# succeed (call to library that is in description)
run_test("deps-in-desc",
  suffix = "-success.R", error_msg = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

# fail (call to library that is not in description)
run_test("deps-in-desc",
  suffix = "-fail.R", error_msg = "Dependency check failed",
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

# with :::
run_test("deps-in-desc",
  "deps-in-desc-dot3",
  suffix = "-fail.R", error_msg = "Dependency check failed",
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc-dot3",
  suffix = "-success.R", error_msg = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc-dot3",
  suffix = "-fail.R", error_msg = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION")),
  cmd_args = "--allow_private_imports"
)

# Rmd
run_test("deps-in-desc",
  "deps-in-desc",
  suffix = "-fail.Rmd", error_msg = "Dependency check failed",
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc",
  suffix = "-success.Rmd", error_msg = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

# README.Rmd is excluded
run_test("deps-in-desc",
  "README.Rmd",
  suffix = "", error_msg = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION-no-deps.dcf"))
)



# Rnw
run_test("deps-in-desc",
  "deps-in-desc",
  suffix = "-fail.Rnw", error_msg = "Dependency check failed",
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc",
  suffix = "-success.Rnw", error_msg = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

# Rprofile
# because .Rprofile is executed on startup, this must be an installed
# package (to not give an error staight away) not listed in
# test_path("in/DESCRIPTION")
if (Sys.getenv("GITHUB_WORKFLOW") != "Hook tests") {
  # seems like .Rprofile with renv activation does not get executed when
  # argument to Rscript contains Rprofile ?! Skip this
  expect_true(rlang::is_installed("R.cache"))
  run_test("deps-in-desc",
    "Rprofile",
    suffix = "", error_msg = "Dependency check failed",
    artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION")),
    file_transformer = function(files) {
      writeLines("R.cache::findCache", files)
      fs::file_move(
        files,
        fs::path(fs::path_dir(files), paste0(".", fs::path_file(files)))
      )
    }
  )

  run_test("deps-in-desc",
    "Rprofile",
    suffix = "", error_msg = NULL,
    artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION")),
    file_transformer = function(files) {
      writeLines("utils::head", files)
      fs::file_move(
        files,
        fs::path(fs::path_dir(files), paste0(".", fs::path_file(files)))
      )
    }
  )
}


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

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### roxygenize                                                              ####
run_test("roxygenize",
  file_name = c("R/roxygenize.R" = "roxygenize.R"),
  suffix = "",
  error_msg = NULL,
  msg = "Writing flie.Rd",
  artifacts = c(
    "DESCRIPTION" = test_path("in/DESCRIPTION-no-deps.dcf")
  ),
  file_transformer = function(files) {
    git_init()
    git2r::add(path = files)
    files
  }
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### readme-rmd-rendered                                                     ####
if (has_git()) {
  run_test("readme-rmd-rendered",
    file_name = c("README.md", "README.Rmd"),
    suffix = "",
    error_msg = "out of date",
    msg = NULL,
    file_transformer = function(files) {
      if (length(files) > 1) {
        # transformer is called once on all files and once per file
        content_2 <- readLines(files[2])
        Sys.sleep(2)
        writeLines(content_2, files[2])
        git_init()
        git2r::add(path = files)
      }
      files
    }
  )


  # only one file staged
  run_test("readme-rmd-rendered",
    file_name = c("README.Rmd", "README.md"),
    suffix = "",
    error_msg = "should be both staged",
    msg = NULL,
    file_transformer = function(files) {
      if (length(files) > 1) {
        # transformer is called once on all files and once per file
        content_2 <- readLines(files[2])
        Sys.sleep(2)
        writeLines(content_2, files[2])
        git_init()
        git2r::add(path = files[1])
      }
      files
    }
  )

  # only has md
  run_test("readme-rmd-rendered",
    file_name = "README.md",
    suffix = "",
    error_msg = NULL,
    msg = NULL,
    file_transformer = function(files) {
      git_init()
      git2r::add(path = files[1])
      files
    }
  )

  # only has Rmd
  run_test("readme-rmd-rendered",
    file_name = "README.Rmd",
    suffix = "",
    error_msg = NULL,
    msg = NULL,
    file_transformer = function(files) {
      git_init()
      git2r::add(path = files[1])
      files
    }
  )
}
