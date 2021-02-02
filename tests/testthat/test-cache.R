test_that("R api works", {
  withr::with_options(
    list(R.cache.rootPath = fs::path_temp(".Rcache")),
    {
      expect_false(has_persistent_R.cache())
    }
  )
  withr::with_options(
    list(R.cache.rootPath = fs::path_temp(".Rcache")),
    {
      expect_output(may_require_permanent_cache(), "This means you")
    }
  )

  withr::with_options(
    list(R.cache.rootPath = fs::path_temp(".Rcache")),
    {
      expect_output(
        may_require_permanent_cache(temporary_cache = TRUE),
        "Using temporary cache at"
      )
    }
  )
})

test_that("CLI API works for style-files", {
  R.cache_root <- fs::path_temp("R.cache")
  fs::dir_create(R.cache_root)
  run_test(
    "style-files",
    suffix = "-cache-success.R",
    env = paste0("R_CACHE_ROOTPATH=", R.cache_root),
    msg = "You can silent this"
  )
  R.cache_root <- fs::dir_create("R.cache")
  fs::dir_create(R.cache_root)
  withr::defer(fs::dir_delete(R.cache_root))
  run_test(
    "style-files",
    suffix = "-cache-success.R",
    env = paste0("R_CACHE_ROOTPATH=", R.cache_root),
    msg = "Using persistant"
  )
})

test_that("CLI API works for roxygenize", {
  R.cache_root <- fs::path_temp("R.cache")
  fs::dir_create(R.cache_root)
  # run_test("deps-in-desc",
  #          "deps-in-desc-dot3",
  #          suffix = "-fail.R", error_msg = "Dependency check failed",
  #          copy = c("DESCRIPTION" = test_path("in/DESCRIPTION"))
  # )
  run_test(
    "roxygenize",
    suffix = "-cache-success.R",
    env = paste0("R_CACHE_ROOTPATH=", R.cache_root),
    msg = "You can silent this",
    copy = c("DESCRIPTION" = test_path("in/DESCRIPTION")),
    file_transformer = function(x) {
      git2r::init()
      x
    }
  )
  R.cache_root <- fs::dir_create("R.cache")
  fs::dir_create(R.cache_root)
  withr::defer(fs::dir_delete(R.cache_root))
  run_test(
    "roxygenize",
    suffix = "-cache-success.R",
    env = paste0("R_CACHE_ROOTPATH=", R.cache_root),
    msg = "Using persistant",
    file_transformer = function(x) {
      git2r::init()
      x
    }
  )
})
