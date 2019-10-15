install_system <- function() {
  if (!is_installed()) {
    usethis::ui_info(paste(
      "Installing pre-commit with conda into the virtual environment",
      "`r-reticulate`."
    ))
    install_precommit_impl()
    usethis::ui_done("Sucessfully installed pre-commit on your system.")
    usethis::ui_todo(
      "To use it with this project, run `precommit::use_precommit()`"
    )
    options(precommit.executable = derive_path_precommit_exec())
  } else {
    path_exec <- find_pre_commit_exec(check_if_exists = FALSE)
    usethis::ui_info(c(
      "pre-commit already installed at the following locations:",
      paste0("- ", path_exec)
    ))
  }
}

#' Install pre-commit on your system.
#'
#' This installs pre-commit in the conda environment r-reticulate. It
#' will be available to use accross different git repositories.
#' @export
install_precommit <- function() {
  install_system()
}

#' Unistall pre-commit
#'
#' @param scope Either "repo" or "global". "repo" removes pre-commit from your
#'   project, but you will be able to use it in other projects. With "global",
#'   you remove the pre-commit executable in the virtual python environment
#'   r-reticulate so it won't be available in any project.
#' @param path_root
uninstall_precommit <- function(scope = "repo",
                                ask = "global",
                                path_root = ".") {
  rlang::arg_match(scope, c("repo", "global"))
  rlang::arg_match(ask, c("repo", "global", "both", "none"))
  withr::with_dir(
    path_root,
    {
      if (scope == "repo") {
        uninstall_precommit_repo(ask = (ask %in% c("repo", "both")))
        path_config <- ".pre-commit-config.yaml"
        if (fs::file_exists(path_config)) {
          fs::file_delete(path_config)
          usethis::ui_done("Removed .pre-commit-config.yaml")
        }
      } else if (scope == "global") {
        uninstall_precommit_system(ask = (ask %in% c("global", "both")))
      }
    }
  )
}

uninstall_precommit_system <- function(ask = TRUE) {
  if (is_installed()) {
    if (ask) {
      answer <- readline(paste(
        "You are about to uninstall pre-commit from the conda env r-reticulate.",
        "It won't be available to any git repo anymore. Do you want to",
        "proceed? You can re-install at any time later with",
        "`precommit::install_precommit()`.",
        "Type 'yes' to continue, 'no' to abort."
      ))
    } else {
      answer <- "yes"
    }

    if (trimws(tolower(answer)) == "yes") {
      is_conda_installation <- grepl(
        "anaconda[0-9]/envs/.*/bin/pre-commit",
        getOption("precommit.executable")
      )
      if (!is_conda_installation) {
        rlang::abort(paste0(
          "R option `precommit.executable` points to ",
          getOption("precommit.executable"),
          " from where we try to uninstall. ",
          "Can only uninstall when installed with conda. ",
          "Please remove manually or set the R option to the appropriate ",
          "executable that lives in a conda environment. You find it with",
          "`precommit::find_pre_commit_exec()`."
        ))
      } else {
        out <- system2(
          reticulate::conda_binary(),
          "remove -n r-reticulate pre-commit --yes"
        )
        if (out == 0) {
          usethis::ui_done("Removed pre-commit from conda env r-reticulate.")
        }
      }
    } else {
      usethis::ui_info("You did not type 'yes', uninstallation process aborted.")
    }
  } else {
    rlang::abort(paste(
      "No installation found, cannot uninstall. Check if the R option",
      "`precommit.executable` points to where you expect the executable."
    ))
  }
}

uninstall_precommit_repo <- function(ask) {
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
    success <- grepl(
      "pre-commit uninstalled",
      system2(find_pre_commit_exec(), "uninstall", stdout = TRUE)
    )
    if (isTRUE(success)) {
      usethis::ui_done("Uninstalled pre-commit from repo scope.")
    }
    if (is_package(".")) {
      lines <- readLines(".Rbuildignore")
      pre_commit_hooks_idx <- which(lines == "^\\.pre-commit-hooks\\.yaml$")
      remaining <- rlang::seq2(1, length(lines)) %>% setdiff(pre_commit_hooks_idx)
      if (length(pre_commit_hooks_idx) > 0) {
        usethis::ui_info("Removing .pre-commit-hooks.yaml from .Rbuildignore")
        usethis::write_over(".Rbuildignore", lines[remaining])
      }
    }
    path_file <- ".pre-commit-config.yaml"
    if (fs::file_exists(path_file)) {
      fs::file_delete(path_file)
      usethis::ui_done("Removed .pre-commit-config.yaml.")
    }

    usethis::ui_info(paste(
      "You can re-install pre-commit for this project at anytime with",
      "`precommit::use_precommit()`."
    ))
  } else {
    usethis::ui_info("You did not type 'yes', uninstallation process aborted.")
  }
}

install_repo <- function() {
  system2(find_pre_commit_exec(), "install")
  usethis::ui_done("Sucessfully installed pre-commit for this repo.")
}

is_installed <- function() {
  fs::file_exists(find_pre_commit_exec(check_if_exists = FALSE))
}
