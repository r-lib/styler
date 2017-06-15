get_stage("after_success") %>%
  add_step(step_hello_world()) %>%
  add_step(step_run_code(covr::codecov()))

get_stage("deploy") %>%
  add_step(step_install_ssh_keys()) %>%
  add_step(step_add_to_known_hosts("github.com")) %>%
  add_step(step_test_ssh())

if (ci()$is_tag() && Sys.getenv("BUILD_PKGDOWN") != "") {
  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(path = "docs", branch = "gh-pages"))
}
