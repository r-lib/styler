#' Create a new release on GitHub
#'
#' This must be done **before** a CRAN release.
#' @param bump The bump increment, either "dev", "patch", "minor" or "major".
#' @param is_cran Is this release a CRAN release?
#' @details
#' This function does the following:
#' - bump description.
#' - update default config in inst/
#' - commit
#' - git tag
#' - run `inst/hooks/local/consistent-release-tag.R` hook with --release-mode (passing args to hooks
#'   not possible interactively, hence we run in advance).
#' - commit and push with skipping `inst/hooks/local/consistent-release-tag.R`.
#' - autoupdate own config file
#' - bump description with dev
#' - commit and push DESCRIPTION and .pre-commit-config.yaml
#'
#' @section CRAN release:
#' If `is_cran` is `TRUE`, the workflow is changed slightly:
#' - push to release branch, not main.
#' - doesn't run [release_complete()]. This must be done manually after accepted
#'   on CRAN.
#' @keywords internal
release_gh <- function(bump = "dev", is_cran = bump != "dev") {
  new_dsc <- release_prechecks(bump, is_cran)
  new_dsc$write()
  git_branch_set(is_cran)
  if (!is_cran) {
    on.exit(sys_call("git", c("checkout", "main")), add = TRUE)
  }

  # if we fail, must reset version, if we succeed, it's not stage
  # on.exit(sys_call("git", c("reset", "HEAD", '--hard')), add = TRUE)

  new_version <- paste0("v", as.character(new_dsc$get_version()))
  cli::cli_alert_success("Bumped version.")
  path_template_config <- c(
    "inst/pre-commit-config-pkg.yaml",
    "inst/pre-commit-config-proj.yaml"
  )

  purrr::walk(path_template_config, update_rev_in_config,
    new_version = new_version
  )
  last_release <- call_and_capture("git", "tag -l --sort=-taggerdate")$stdout[1]

  cli::cli_alert_success("Updated version in default config.")
  msg1 <- glue::glue("Release {new_version}, see NEWS.md for details.")
  msg2 <- glue::glue(
    "Diff to previous release: ",
    "https://github.com/lorenzwalthert/precommit/compare/",
    "{last_release}...{new_version}"
  )
  sys_call(
    "git",
    glue::glue(
      'commit DESCRIPTION {paste0(path_template_config, collapse = " ")} ',
      '-m "{msg1}" -m "{msg2}"'
    ),
    env = "SKIP=spell-check,consistent-release-tag"
  )
  cli::cli_alert_success("Committed DESCRIPTION and config template")
  sys_call("git", glue::glue('tag -a {new_version} -m "{msg1}" -m "{msg2}"'))
  sys_call("./inst/hooks/local/consistent-release-tag.R", "--release-mode")
  cli::cli_alert_success("Tagged last commit with release version.")
  if (!is_cran) {
    sys_call("git", glue::glue("push origin {new_version}"),
      env = "SKIP=consistent-release-tag"
    )
  }

  sys_call("git", glue::glue("push"),
    env = "SKIP=consistent-release-tag"
  )
  cli::cli_alert_success("Pushed commits and tags.")
  if (is_cran) {
    cli::cli_alert_info(paste(
      "Once on CRAN, call `precommit::release_complete(is_cran = TRUE)` to bump to the devel",
      "version."
    ))
  } else {
    release_complete(ask = FALSE, tag = new_version, is_cran = FALSE)
  }
}

#' Complete the release
#'
#' Bumps the version to devel.
#' @param tag The tag to push. `NULL` will derive the tag from `DESCRIPTION`.
#' @keywords internal
release_complete <- function(ask = TRUE, is_cran = ask, tag = NULL) {
  if (git_branch_get() != "main") {
    rlang::abort("Must be on main to complete the release.")
  }
  if (ask) {
    abort_if_not_yes("Did you merge the release branch into main?")
    abort_if_not_yes("Did you pull the latest main from origin?")
  }
  if (is_cran) {
    if (is.null(tag)) {
      tag <- paste0("v", desc::desc_get_version())
    }
    if (substr(tag, 1, 1) != "v") {
      rlang::abort("tag must start with v.")
    }
    sys_call("git", glue::glue("push origin {tag}"))
  }
  autoupdate() # only updates if tag is on the main branch
  desc::desc_bump_version("dev")
  cli::cli_alert_success("Bumped version to dev")
  sys_call("git", glue::glue('commit DESCRIPTION .pre-commit-config.yaml -m "use latest release"'),
    env = "SKIP=spell-check"
  )
  sys_call("git", glue::glue("push"))
  cli::cli_alert_success("Committed and pushed dev version.")
  if (is_cran) {
    cli::cli_ul("Go to GitHub and draft a new release from the tag {tag}.")
  }
}


release_prechecks <- function(bump, is_cran) {
  git_assert_clean()
  cli::cli_alert_info("autoupdating hooks.")
  autoupdate()
  git_assert_clean()
  old_version <- paste0("v", desc::desc_get_version())
  dsc <- desc::description$new()
  suppressMessages(dsc$bump_version(bump))
  new_version <- paste0("v", dsc$get_version())
  abort_if_not_yes("Your target release has version {new_version}, correct?")
  abort_if_not_yes("Did you prepare NEWS.md for this version ({new_version})?")
  dsc
}

#' Updates the hook version ref of {precommit} in a `.pre-commit-config` file
#'
#' This is useful in the release process because when releasing a new version,
#' we must make sure the template that is used with `precommit::use_precommit()`
#' is up-to date. Also, after we pushed the release to GitHub, we want to update
#' the hooks from our own hook repo in the source repo too (we could also do that
#' with `precommit::autoupdate()` though).
#' @param new_version The version string of the new version.
#' @param path The path to a pre-commit config file.
#' @keywords internal
update_rev_in_config <- function(new_version,
                                 path = "inst/pre-commit-config.yaml") {
  config <- readLines(path)
  ours <- grep(paste0("-   repo: ", hooks_repo), config, fixed = TRUE)
  others <- setdiff(grep("-   repo:", config, fixed = TRUE), ours)
  next_after_ours <- others[others > ours][1]
  rev <- grep("rev:", config)
  our_rev <- rev[rev > ours & rev < next_after_ours]

  our_rev_without_comments <- gsub("#.*", "", config[our_rev])
  if (any(grepl("#", config[our_rev]))) {
    rlang::warn("removed comment on rev. Please add manually")
  }
  old_version <- trimws(gsub(".*rev:", "", our_rev_without_comments))
  if (old_version == new_version) {
    message("Nothing to do, old version and new version are identical.")
  } else {
    message(glue::glue("Replacing {old_version} with {new_version} in {path}"))
    config[our_rev] <- gsub(old_version, new_version, config[our_rev])
    writeLines(config, path)
  }
}

abort_if_not_yes <- function(...) {
  answer <- usethis::ui_yeah(..., .envir = parent.frame(n = 1))
  if (!answer) {
    rlang::abort("Assertion failed")
  }
}


sys_call <- function(...) {
  status <- system2(...)
  if (status != 0) {
    rlang::abort("system2 failed.")
  }
}

git_branch_set <- function(is_cran) {
  if (is_cran) {
    branch_name <- paste0("rc-v", desc::desc_get_version())

    sys_call(
      "git",
      c(
        "checkout",
        if (!(branch_name %in% names(git2r::branches()))) "-b",
        branch_name
      )
    )
    cli::cli_alert_success("Checked out branch {branch_name} for CRAN release process.")
  } else {
    if (git_branch_get() != "main") {
      rlang::abort(paste(
        "Need to be on branch 'main' to create a release, otherwise autoudate",
        "won't use the new ref."
      ))
    }
    git_assert_even_with_origin()
  }
}

git_assert_even_with_origin <- function() {
  tmp <- tempfile()
  system2("git", "diff HEAD..origin/main", stdout = tmp)
  if (length(readLines(tmp) > 0)) {
    rlang::abort("remote main must be even with local main before release process can start.")
  }
}

git_assert_clean <- function() {
  if (length(unlist(git2r::status()) > 0)) {
    rlang::abort("Need clean git directory before starting release process.")
  }
}

git_branch_get <- function() {
  git2r::repository_head()$name
}
