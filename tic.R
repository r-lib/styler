do_package_checks(codecov = ci_has_env("CODECOV"))

if (ci_has_env("BUILD_PKGDOWN")) {
  do_pkgdown()
}
