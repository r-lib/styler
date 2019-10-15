#' Locate the pre-comit executable
#'
#' @param check_if_exists Whether or not to make sure the returned path also
#'  exists.
#' @export
find_pre_commit_exec <- function(check_if_exists = TRUE) {
  final <- getOption("precommit.executable") %>%
    as.character()
  if (!check_if_exists) {
    return(final)
  }
  if (!fs::file_exists(final)) {
    rlang::abort(paste0(
      "pre-commit executable does not exist at ",
      final,
      ". Please locate your pre-commit ",
      "executable and set the R option `precommit.executable` to this ",
      "path so it can be used to perform various pre-commit commands from R."
    ))
  }
  final
}

derive_path_precommit_exec <- function() {
  tryCatch(
    {
      ls <- reticulate::conda_list()

      cat("ls is:", nrow(ls))
      cat("python is:", ls$python)
      cat("name is:", ls$name)

      path_reticulate <- fs::path_dir(ls[ls$name == "r-reticulate", "python"][1])
      cat("path_reticulate: ", path_reticulate)
      derived <- fs::path(path_reticulate, "pre-commit")
      cat("derived: ", derived)
      out <- unname(ifelse(fs::file_exists(derived), derived, ""))
      cat("derived that exists: ", out)
      out
    },
    error = function(e) ""
  )
}
