get_stage("after_install") %>%
  add_code_step(
    {
      if (isTRUE(as.logical(toupper(Sys.getenv("R_REMOVE_RCACHE"))))) {
        remove.packages("R.cache")
      }
    }
  )
do_package_checks(error_on = ifelse(getRversion() >= "3.2", "note", "error"))

if (Sys.getenv("id_rsa") != "" && ci()$get_branch() == "master" && Sys.getenv("BUILD_PKGDOWN") != "") {
  do_pkgdown(orphan = FALSE)
}
