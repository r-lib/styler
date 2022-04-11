install_system <- function(force) {
  assert_reticulate_is_installed()
  if (!is_installed() | force) {
    cli::cli_alert_info(paste(
      "Installing pre-commit into the conda environment",
      "{.code r-precommit }."
    ))
    install_impl()
    cli::cli_alert_success("Sucessfully installed pre-commit on your system.")
    cli::cli_ul(
      "To use it with this project, run `precommit::use_precommit()`"
    )
    path_exec <- path_derive_precommit_exec()
    options(precommit.executable = path_exec)
  } else {
    path_exec <- path_precommit_exec(check_if_exists = FALSE)
    cli::cli_alert_info(paste0(
      "pre-commit already installed at the following locations:\n\n",
      paste0("- ", path_exec), "\n\nUse `precommit::update_precommit()` to ",
      "update the executable."
    ))
  }
  invisible(path_exec)
}

assert_reticulate_is_installed <- function() {
  if (!rlang::is_installed("reticulate")) {
    rlang::abort(paste(
      "Please install the R package reticulate to use this installation",
      "method. You can also use alternative installation methods that don't",
      "require reticulate. See https://lorenzwalthert.github.io/precommit."
    ))
  }
}

#' Install pre-commit on your system
#'
#' This installs pre-commit in the conda environment r-precommit. It
#' will be available to use across different git repositories. To update,
#' refer to [update_precommit()].
#' @param force Whether or not to force a re-installation.
#' @return
#' The path to the pre-commit executable (invisibly).
#' @family executable managers
#' @examples
#' \dontrun{
#' install_precommit()
#' }
#' @export
install_precommit <- function(force = FALSE) {
  install_system(force = force)
}

#' Install pre-commit on your system with conda
#' @keywords internal
install_impl <- function() {
  if (!"r-precommit" %in% reticulate::conda_list()$name) {
    reticulate::conda_create("r-precommit")
  }
  reticulate::conda_install("r-precommit", packages = "pre-commit")
}

#' Updates pre-commit on your system with conda
#' @keywords internal
update_impl <- function() {
  system2(
    reticulate::conda_binary(), c(
      "update", "--yes", "-n", "r-precommit",
      "-c", "defaults", "-c", "conda-forge",
      "pre-commit"
    )
  )
}

install_repo <- function(root, install_hooks, legacy_hooks) {
  wait_for_hook_installation <- getOption(
    "precommit.block_install_hooks", FALSE
  )
  withr::with_dir(root, {
    remove_usethis_readme_hook()
    out <- call_precommit(
      "install",
      if (install_hooks) "--install-hooks",
      if (legacy_hooks == "remove") "--overwrite",
      wait = wait_for_hook_installation
    )
    if (out$exit_status == 0) {
      if (any(grepl("Use -f to use only pre-commit.", out$stdout, fixed = TRUE))) {
        if (legacy_hooks == "forbid") {
          rlang::abort(paste(
            "There are existing hooks installed for this repo and the argument",
            "`legacy_hooks` is set to `'forbid'`. We recommend inspecting these",
            "and removing them manually or - if you are sure you don't need",
            "them anymore - call this function again with",
            "`legacy_hooks = 'remove'` to remove them for you.",
            "If you want to continue to use these hooks, set",
            "`legacy_hooks` to `'allow'`, which means pre-commit will run in ",
            "legacy mode and run these hooks as well as pre-commit hooks."
          ))
          call_precommit("uninstall")
        } else if (legacy_hooks == "allow") {
          cli::cli_alert_success(paste(
            "Sucessfully installed pre-commit for repo.",
            "Existing hooks found and argument {.code allow_legacy = TRUE }. Running",
            "in migration mode."
          ))
        }
      } else {
        if (wait_for_hook_installation) {
          cli::cli_alert_success("Sucessfully installed pre-commit for repo.")
        } else {
          cli::cli_alert_info(paste0(
            "Installing hooks in non-blocking background process.",
            " If you experience problems or prefer a blocking process, use ",
            '{.code options("precommit.block_install_hooks" = TRUE)}.'
          ))
        }
      }
    } else {
      cli::cli_alert_danger("Failed to install pre-commit for repo.")
      communicate_captured_call(out, preamble = "Problems during initialization:")
    }
  })
}

remove_usethis_readme_hook <- function() {
  legacy <- readLines(
    system.file("usethis-legacy-hook", package = "precommit"),
    encoding = "UTF-8"
  )
  candidate <- ".git/hooks/pre-commit"
  if (file_exists(candidate)) {
    if (identical(readLines(candidate, encoding = "UTF-8"), legacy)) {
      fs::file_delete(candidate)
      cli::cli_alert_info(paste(
        "Removed the render-README hook, which was added with",
        "`usethis::use_readme_rmd()` to this repo at some point in the past.",
        "{{precommit}}'s equivalent is the hook with the id 'readme-rmd-rendered'.",
        "Add the hook to your .pre-commit-config.yaml as described here:",
        "https://lorenzwalthert.github.io/precommit/#usage."
      ))
    }
  }
}


#' Uninstall pre-commit
#'
#' Remove pre-commit from a repo or from your system.
#' @param scope Either "repo" or "user". "repo" removes pre-commit from your
#'   project, but you will be able to use it in other projects. With "user",
#'   you remove the pre-commit executable in the virtual python environment
#'   r-precommit so it won't be available in any project. When you want to do
#'   the latter, you should first do the former.
#' @param ask Either "user", "repo" or "none" to determine in which case
#'   a prompt should show up to let the user confirm his action.
#' @inheritParams fallback_doc
#' @return
#' `NULL` (invisibly). The function is called for its side effects.
#' @family executable managers
#' @examples
#' \dontrun{
#' uninstall_precommit()
#' }
#' @export
uninstall_precommit <- function(scope = "repo",
                                ask = "user",
                                root = here::here()) {
  rlang::arg_match(scope, c("repo", "user"))
  rlang::arg_match(ask, c("repo", "user", "both", "none"))
  withr::with_dir(root, {
    if (scope == "repo") {
      uninstall_repo(ask = (ask %in% c("repo", "both")))
      path_config <- ".pre-commit-config.yaml"
      if (file_exists(path_config)) {
        fs::file_delete(path_config)
        cli::cli_alert_success("Removed .pre-commit-config.yaml")
      }
    } else if (scope == "user") {
      uninstall_system(ask = (ask %in% c("user", "both")))
    }
  })
  invisible(NULL)
}

uninstall_system <- function(ask = TRUE) {
  if (is_installed()) {
    if (ask) {
      answer <- readline(paste(
        "You are about to uninstall pre-commit from the conda env r-precommit.",
        "It won't be available to any git repo anymore. Do you want to",
        "proceed? You can re-install at any time later with",
        "`precommit::install_precommit()`.",
        "Type 'yes' to continue, 'no' to abort."
      ))
    } else {
      answer <- "yes"
    }

    if (trimws(tolower(answer)) == "yes") {
      if (!is_conda_installation()) {
        rlang::abort(paste0(
          "R option `precommit.executable` points to ",
          getOption("precommit.executable"),
          " from where we try to uninstall. ",
          "Can only uninstall when installed with conda into env r-precommit. ",
          "Please remove pre-commit manually from the command line. "
        ))
      } else {
        if (!rlang::is_installed("reticulate")) {
          rlang::abort("Must install the R package reticulate to use this functionality.")
        }
        out <- call_and_capture(
          reticulate::conda_binary(),
          "remove -n r-precommit pre-commit --yes"
        )
        if (out$exit_status == 0) {
          cli::cli_alert_success("Removed pre-commit from conda env r-precommit.")
        } else {
          communicate_captured_call(out)
        }
      }
    } else {
      rlang::abort("You did not type 'yes', uninstallation process aborted.")
    }
  } else {
    rlang::abort(paste(
      "No installation found, cannot uninstall. Check if the R option",
      "`precommit.executable` points to where you expect the executable."
    ))
  }
}

uninstall_repo <- function(ask) {
  if (ask) {
    answer <- readline(paste0(
      "Are you sure you want to remove pre-commit from this repo? ",
      "Then type 'yes'."
    ))
    if (trimws(tolower(answer)) == "yes") {
      continue <- TRUE
    }
  } else {
    continue <- TRUE
  }
  if (continue) {
    out <- call_precommit("uninstall")
    if (out$exit_status == 0) {
      cli::cli_alert_success("Uninstalled pre-commit from repo scope.")
    } else {
      communicate_captured_call(out)
    }
    if (is_package(".")) {
      lines <- readLines(".Rbuildignore", encoding = "UTF-8")
      precommit_hooks_idx <- which(lines == "^\\.pre-commit-config\\.yaml$")
      remaining <- rlang::seq2(1, length(lines)) %>% setdiff(precommit_hooks_idx)
      if (length(precommit_hooks_idx) > 0) {
        cli::cli_alert_info("Removing .pre-commit-hooks.yaml from .Rbuildignore")
        writeLines(enc2utf8(lines[remaining]), ".Rbuildignore")
      }
    }
    path_file <- ".pre-commit-config.yaml"
    if (file_exists(path_file)) {
      fs::file_delete(path_file)
      cli::cli_alert_success(paste(
        "Removed .pre-commit-config.yaml. If you want your collaborators",
        "to be able to\ncontinue to use pre-commit in this repo, you should",
        "undo the deletion of this file,\ne.g. with `$ git checkout",
        ".pre-commit-config.yaml`."
      ))
    }

    cli::cli_alert_info(paste(
      "You can re-install pre-commit for this project at anytime with",
      "`precommit::use_precommit()`."
    ))
  } else {
    cli::cli_alert_info("You did not type 'yes', uninstallation process aborted.")
  }
}

is_installed <- function() {
  file_exists(path_precommit_exec(check_if_exists = FALSE))
}
