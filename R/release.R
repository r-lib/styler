#' Updates the hook version ref of {precommit} in a `.pre-commit-config` file
#' 
#' This is useful in the release process because when releasing a new version, 
#' we must make sure the template that is used with `precommit::use_precommit()`
#' is up-to date. Also, after we pushed the release to GitHub, we want to update
#' the hooks from our own hook repo in the source repo too (we could also do that
#' with `precommit::autoupdate()` though).
#' @apram new_version The version string of the new version.
#' @param path The path to a pre-commit config file. 
#' @keywords internal
update_rev_in_config <- function(new_version,
                                 path = "inst/pre-commit-config.yaml") {
  config <- readLines(path)
  ours <- grep("-   repo: https://github.com/lorenzwalthert/precommit", config, fixed = TRUE)
  others <- setdiff(grep("-   repo:", config, fixed = TRUE), ours)
  next_after_ours <- others[others > ours]
  rev <- grep("rev:", config)
  our_rev <- rev[rev > is_ours & rev < next_after_ours]

  our_rev_without_comments <- gsub("#.*", "", config[our_rev])
  if (grepl("#", config[our_rev])) {
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
