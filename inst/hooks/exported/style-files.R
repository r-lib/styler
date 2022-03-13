#!/usr/bin/env Rscript
'style files.
Usage:
  style_files [--style_pkg=<style_guide_pkg>] [--style_fun=<style_guide_fun>] [--cache-root=<cache_root_>] [--no-warn-cache] [--ignore-start=<ignore_start_>] [--ignore-stop=<ignore_stop_>] <files>...

Options:
  --style_pkg=<style_guide_pkg>  Package where the style guide is stored [default: styler].
  --style_fun=<style_guide_fun>  The styling function in style_pkg [default: tidyverse_style].
  --no-warn-cache  Suppress the warning about a missing permanent cache.
  --cache-root=<cache_root_> Passed to `options("styler.cache_root")` [default: styler-perm].
  --ignore-start=<ignore_start_> Passed to `options("styler.ignore_start")`.
  --ignore-stop=<ignore_stop_> Passed to `options("styler.ignore_stop")`.
' -> doc

if (packageVersion("precommit") < "0.1.3.9010") {
  rlang::abort(paste(
    "This hooks only works with the R package {precommit} >= 0.1.3.9010",
    'Please upgrade with `remotes::install_github("lorenzwalthert/precommit@v0.1.3.9010")`.'
  ))
}

# the code bellow will basically expand `doc` with the additional key values
# provided passed to `style_text(...), passed as --key=value. This way, we
# can rely on {docopt} to do the parsing for us afterwards.
# We'll use yaml.load() to convert from string to numeric/logical.
library(styler)
args <- commandArgs(trailingOnly = TRUE)
non_file_args <- args[!grepl("^[^-][^-]", args)]
keys <- setdiff(
  gsub("(^--[0-9A-Za-z_-]+).*", "\\1", non_file_args),
  c("--style_pkg", "--style_fun", "--cache-root", "--no-warn-cache", "--ignore-start", "--ignore-stop")
)
if (length(keys) > 0) {
  bare_keys <- gsub("^--", "", keys)
  key_value_pairs <- paste0("  ", keys, "=<default_", bare_keys, ">  non_file_args ", bare_keys, ".")
  insert <- paste(paste0("[", keys, "=<default_", bare_keys, ">]", collapse = " "), "<files>...")

  doc <- gsub("<files>...", insert, paste0(doc, paste(key_value_pairs, collapse = "\n")))
}

arguments <- docopt::docopt(doc, args)
if (packageVersion("styler") < "1.3.2") {
  stop(
    "Your styler version is outdated. ",
    "Please install a newer version of styler with ",
    "`install.packages('styler')`. This will get you big speed-ups."
  )
} else {
  precommit::may_require_permanent_cache(arguments$no_warn_cache)
}
options("styler.cache_root" = arguments$cache_root)
if (!is.null(arguments$ignore_start)) {
  options("styler.ignore_start" = arguments$ignore_start)
}
if (!is.null(arguments$ignore_stop)) {
  options("styler.ignore_stop" = arguments$ignore_stop)
}
print(c("cache root set to ", arguments$cache_root))
if (!rlang::is_installed(arguments$style_pkg)) {
  rlang::abort(paste0(
    "{", arguments$style_pkg,
    "} must be listed in `additional_dependencies:` with a syntax that `renv::install()` understands, e.g. for a public GitHub package, adapt your `.pre-commit-config.yaml` file:

    -   id: style-files
        args: [--style_pkg=", arguments$style_pkg, ", --style_fun=", arguments$style_fun, "]
        additional_dependencies:
        -     github-org/repo-with-package

See 'Examples' in `help(\"install\", package = \"renv\")` for other supported ways of specifying the dependency.
"
  ))
}

style <- eval(parse(text = paste(arguments$style_pkg, "::", arguments$style_fun)))

tryCatch(
  {
    dot_args <- list(path = arguments$files, style = style)
    if (length(keys) > 0) {
      dot_args <- append(dot_args, lapply(arguments[bare_keys], function(x) {
        tryCatch(
          tryCatch(
            # 1 try yaml.load() -> parse
            eval(parse(text = tryCatch(yaml::yaml.load(x),
              # 2 try parse
              error = function(...) x
            ))),
            # 3 try yaml.load
            error = yaml::yaml.load(x)
            # 4 try as is.
          ),
          error = function(...) x
        )
      }))
    }
    do.call(style_file, dot_args)
  },
  warning = function(w) {
    msg <- conditionMessage(w)
    if (grepl("Unknown or uninitialised column", msg, ignore.case = TRUE)) {
      warning(msg, call. = FALSE)
    } else {
      stop(msg, call. = FALSE)
    }
  }
)
