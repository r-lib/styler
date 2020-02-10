do_package_checks(error_on = ifelse(getRversion() >= "3.2", "note", "error"))

if (Sys.getenv("id_rsa") != "" && ci()$get_branch() == "master" && Sys.getenv("BUILD_PKGDOWN") != "") {
  do_pkgdown(orphan = FALSE)
}
