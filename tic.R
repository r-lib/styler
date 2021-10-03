Sys.setenv(LINTR_COMMENT_BOT = "false")
do_package_checks(
  error_on = ifelse(tic::ci_has_env("EXTERNAL_INSTALLATION"), "warning", "note")
)

if (ci_has_env("BUILD_PKGDOWN")) {
  do_pkgdown()
}
