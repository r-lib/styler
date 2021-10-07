test_that("R api works", {
  withr::with_options(
    list(R.cache.rootPath = fs::file_temp(".Rcache")),
    {
      expect_false(has_persistent_R.cache())
    }
  )
  withr::with_options(
    list(R.cache.rootPath = fs::file_temp(".Rcache")),
    {
      expect_output(may_require_permanent_cache(), "This means you")
    }
  )

  withr::with_options(
    list(R.cache.rootPath = fs::file_temp(".Rcache")),
    {
      expect_output(
        may_require_permanent_cache(temp_cache_is_enough = TRUE),
        "Using temporary cache at"
      )
    }
  )
})

test_that("CLI API works for style-files", {
  skip_if(is_windows(), "env not supported in system2 on Windows")
  R.cache_root <- fs::path_abs(fs::file_temp("R.cacheTemp"))
  fs::dir_create(R.cache_root)
  run_test(
    "style-files",
    suffix = "-cache-success.R",
    env = paste0("R_CACHE_ROOTPATH=", R.cache_root),
    msg = "You can silent this"
  )
  R.cache_root <- fs::path_abs(fs::dir_create("R.cachePerm"))
  fs::dir_create(R.cache_root)
  withr::defer(fs::dir_delete(R.cache_root))
  run_test(
    "style-files",
    suffix = "-cache-success.R",
    env = paste0("R_CACHE_ROOTPATH=", R.cache_root),
    msg = "Using persistent"
  )
})

test_that("CLI API works for roxygenize", {
  skip_if(is_windows(), "env not supported in system2 on Windows")
  R.cache_root <- fs::path_abs(fs::file_temp("R.cacheTemp"))
  fs::dir_create(R.cache_root)
  run_test(
    "roxygenize",
    suffix = "-cache-success.R",
    env = paste0("R_CACHE_ROOTPATH=", R.cache_root),
    msg = "You can silent this",
    artifacts = c("DESCRIPTION" = test_path("in/DESCRIPTION-no-deps.dcf")),
    file_transformer = function(x) {
      git2r::init()
      x
    }
  )
  R.cache_root <- fs::path_abs(fs::dir_create("R.cachePerm"))
  fs::dir_create(R.cache_root)
  withr::defer(fs::dir_delete(R.cache_root))
  run_test(
    "roxygenize",
    suffix = "-cache-success.R",
    env = paste0("R_CACHE_ROOTPATH=", R.cache_root),
    msg = "Using persistent",
    file_transformer = function(x) {
      git2r::init()
      x
    }
  )
})
