cat("In tests/testthat/helpers-devel-options: ")
cache_deactivate()

styler_version <- utils::packageDescription("styler", fields = "Version")
clear_testthat_cache <- purrr::partial(cache_clear, "testthat", ask = FALSE)
activate_testthat_cache <-  purrr::partial(cache_activate, "testthat")
