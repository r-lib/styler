is_package <- function(base_path = here::here()) {
  res <- tryCatch(rprojroot::find_package_root_file(path = base_path),
                  error = function(e) NULL
  )
  !is.null(res)
}
