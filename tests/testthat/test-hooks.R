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
  std_err = NA
)

run_test("style-files",
  suffix = "-fail-parse.R", cmd_args = c("--cache-root=styler"),
  std_err = "unexpected"
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

run_test("style-files",
  file_name = "style-files",
  suffix = "-ignore-success.R",
  cmd_args = c(
    "--cache-root=styler",
    '--ignore-start="^# styler: off$"',
    '--ignore-stop="^# styler: on$"'
  )
)

run_test("style-files",
  file_name = "style-files",
  suffix = "-ignore-fail.R",
  cmd_args = "--cache-root=styler",
  std_err = "Invalid stylerignore sequences"
)


# fail with cmd args
run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-success.R",
  std_err = "scope must be one",
  cmd_args = c("--scope=space", "--cache-root=styler")
)

run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-fail.R",
  std_err = NA,
  cmd_args = c(
    "--style_pkg=styler", "--style_fun=tidyverse_style", "--cache-root=styler"
  )
)

run_test("style-files",
  file_name = "style-files-cmd",
  suffix = "-fail.R",
  std_err = "must be listed in `additional_dependencies:`",
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
  std_err = NULL
)

# failure
run_test(
  "no-browser-statement",
  suffix = "-fail.R",
  std_err = "contains a `browser()` statement."
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### no-debug-statement                                                    ####
# success
run_test(
  "no-debug-statement",
  suffix = "-success.R",
  std_err = NULL
)

# failure
run_test(
  "no-debug-statement",
  suffix = "-fail.R",
  std_err = "contains a `debug()` or `debugonce()` statement."
)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### parsable-R                                                              ####

# success
run_test("parsable-R",
  suffix = "-success.R",
  std_err = NULL
)

run_test("parsable-R",
  suffix = "-success.Rmd",
  std_err = NULL
)

# failure
run_test("parsable-R", suffix = "-fail.R", std_out = "Full context", std_err = "1 1")

run_test(
  "parsable-R",
  suffix = "-fail.Rmd",
  std_out = "parsable-R-fail.Rmd",
  std_err = "1 1"
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### spell-check                                                             ####
# success
run_test("spell-check", suffix = "-success.md", std_err = NULL)

# basic failure
run_test("spell-check", suffix = "-fail.md", std_err = "Spell check failed")

# success with wordlist
run_test("spell-check",
  suffix = "-wordlist-success.md",
  std_err = NULL,
  artifacts = c("inst/WORDLIST" = test_path("in/WORDLIST"))
)

# success with ignored files
# uses lang argument
run_test("spell-check", suffix = "-language-success.md", cmd_args = "--lang=en_GB")



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### depds-in-desc                                                           ####
# succeed (call to library that is in description)
run_test("deps-in-desc",
  suffix = "-success.R", std_err = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

# fail (call to library that is not in description)
run_test("deps-in-desc",
  suffix = "-fail.R", std_err = "Dependency check failed",
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

# with :::
run_test("deps-in-desc",
  "deps-in-desc-dot3",
  suffix = "-fail.R", std_err = "Dependency check failed",
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc-dot3",
  suffix = "-success.R", std_err = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc-dot3",
  suffix = "-fail.R", std_err = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION")),
  cmd_args = "--allow_private_imports"
)

# Rmd
run_test("deps-in-desc",
  "deps-in-desc",
  suffix = "-fail.Rmd", std_err = "Dependency check failed",
  std_out = "in file `deps-in-desc-fail.Rmd`",
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc",
  suffix = "-success.Rmd", std_err = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

# README.Rmd is excluded
run_test("deps-in-desc",
  "README.Rmd",
  suffix = "", std_err = NULL,
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION-no-deps.dcf"))
)



# Rnw
run_test("deps-in-desc",
  "deps-in-desc",
  suffix = "-fail.Rnw", std_err = "Dependency check failed",
  artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
)

run_test("deps-in-desc",
  "deps-in-desc",
  suffix = "-success.Rnw", std_err = NULL,
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
    suffix = "", std_err = "Dependency check failed",
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
    suffix = "", std_err = NULL,
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
  std_err = NULL
)

# failure
run_test("lintr", suffix = "-fail.R", std_err = "not lint free")

# warning
run_test(
  "lintr",
  suffix = "-fail.R", cmd_args = "--warn_only", std_err = NULL
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### roxygenize                                                              ####
# with outdated Rd present
run_test("roxygenize",
  file_name = c("man/flie.Rd" = "flie.Rd"),
  suffix = "",
  std_err = NA,
  std_out = "Writing NAMESPACE",
  artifacts = c(
    "DESCRIPTION" = test_path("in/DESCRIPTION-no-deps.dcf"),
    "R/roxygenize.R" = test_path("in/roxygenize.R")
  ),
  file_transformer = function(files) {
    git_init()
    git2r::add(path = files)
    # hack to add artifact to trigger diff_requires_roxygenize()
    git2r::add(path = fs::path(fs::path_dir(fs::path_dir(files[1])), "R"))
    files
  }
)


# without Rd present
run_test("roxygenize",
  file_name = c("R/roxygenize.R" = "roxygenize.R"),
  suffix = "",
  std_err = "Please commit the new `.Rd` files",
  artifacts = c(
    "DESCRIPTION" = test_path("in/DESCRIPTION-no-deps.dcf")
  ),
  file_transformer = function(files) {
    git_init()
    git2r::add(path = files)
    files
  }
)


# with up to date rd present
run_test("roxygenize",
  file_name = c("man/flie.Rd" = "flie-true.Rd"),
  suffix = "",
  std_err = "Writing NAMESPACE",
  artifacts = c(
    "DESCRIPTION" = test_path("in/DESCRIPTION-no-deps.dcf"),
    "R/roxygenize.R" = test_path("in/roxygenize.R")
  ),
  file_transformer = function(files) {
    git_init()
    git2r::add(path = files)
    # hack to add artifact to trigger diff_requires_roxygenize()
    git2r::add(path = fs::path(fs::path_dir(fs::path_dir(files[1])), "R"))
    files
  },
  expect_success = TRUE
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### codemeata                                                               ####
run_test("codemeta-description-update",
  file_name = c("codemeta.json"),
  suffix = "",
  std_err = "No `DESCRIPTION` found in repository.",
  std_out = NULL,
)

run_test("codemeta-description-update",
  file_name = c("DESCRIPTION"),
  suffix = "",
  std_err = "No `codemeta.json` found in repository.",
  std_out = NULL,
)


# outdated
run_test("codemeta-description-update",
  file_name = c("DESCRIPTION", "codemeta.json"),
  suffix = "",
  std_err = "out of date",
  std_out = NULL,
  file_transformer = function(files) {
    if (length(files) > 1) {
      # transformer is called once on all files and once per file
      content_2 <- readLines(files[1])
      Sys.sleep(2)
      writeLines(content_2, files[1])
    }
    files
  }
)

# succeed
run_test("codemeta-description-update",
  file_name = c("DESCRIPTION", "codemeta.json"),
  suffix = "",
  file_transformer = function(files) {
    if (length(files) > 1) {
      # transformer is called once on all files and once per file
      content_2 <- readLines(files[2])
      Sys.sleep(2)
      writeLines(content_2, files[2])
    }
    files
  }
)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### readme-rmd-rendered                                                     ####
if (has_git()) {
  run_test("readme-rmd-rendered",
    file_name = c("README.md", "README.Rmd"),
    suffix = "",
    std_err = "out of date",
    std_out = NULL,
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
    std_err = "should be both staged",
    std_out = NULL,
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
    std_err = NULL,
    std_out = NULL,
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
    std_err = NULL,
    std_out = NULL,
    file_transformer = function(files) {
      git_init()
      git2r::add(path = files[1])
      files
    }
  )
}
