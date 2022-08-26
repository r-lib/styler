# The script should be written here to benchmark the package on Github.
# NOTE: save_data and save_plot arguments must always be set to TRUE for the PR benchmark comments.

## TEST 1
Rperform::plot_metrics(
    test_path = "tests/testthat/test-rmd.R",
    metric = "time", num_commits = 10,
    save_data = TRUE,
    save_plots = TRUE
)


## TEST 2
# Rperform::plot_metrics(
#     test_path = "inst/tests/test-join.r",
#     metric = "memory", num_commits = 10,
#     save_data = TRUE,
#     save_plots = TRUE
# )

## TEST 3
# Rperform::time_compare(
#    test_path = "inst/tests/test-dup.r",
#    num_commits = 10,
#    save_data = T
# )

## TEST 4
# Rperform::mem_compare(
#    test_path = "inst/tests/test-dup.r",
#    num_commits = 10,
#    save_data = TRUE
# )

## TEST 5
#  Rperform::plot_branchmetrics(
#    test_path = "inst/tests/test-check.r",
#    metric = "time",
#    branch1 = "rperform_test",
#    branch2 = "master",
#    save_data = TRUE,
#    save_plots = TRUE
# )