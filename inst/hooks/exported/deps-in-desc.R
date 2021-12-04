#!/usr/bin/env Rscript

"Ensure all dependencies of the form pkg::fun are in DESCRIPTION
Usage:
  deps-in-desc [--allow_private_imports] <files>...

Options:
  --allow_private_imports  Whether or not to allow the use of ::: on imported functions.
" -> doc
pre_installed <- c(
  "base", "boot", "class", "cluster", "codetools", "compiler",
  "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth",
  "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet",
  "parallel", "rpart", "spatial", "splines", "stats", "stats4",
  "survival", "tcltk", "tools", "utils"
)

arguments <- docopt::docopt(doc)
deps_in_desc <- function(file, arguments) {
  if (basename(file) == "README.Rmd") {
    # is .Rbuildignored, dependencies irrelevant
    return()
  }
  if (grepl("(\\.Rmd|\\.Rnw|\\.rmd|\\.rnw)$", file)) {
    file <- knitr::purl(file, output = tempfile())
  }
  parse_data <- getParseData(parse(file = file, keep.source = TRUE))
  used <- parse_data[parse_data$token == "SYMBOL_PACKAGE", ]$text
  local_pkg_name <- unname(desc::desc_get("Package"))
  unregistered <- setdiff(
    unique(used),
    c(pre_installed, desc::desc_get_deps(file = ".")$package, local_pkg_name)
  )
  out <- TRUE
  if (length(unregistered) > 0) {
    cat(
      "Not all packages used in your code are listed in DESCRIPTION. ",
      "The following are missing in file `", file, "`: ",
      paste(unregistered, collapse = ", "), ".\n",
      "You can add them with `usethis::use_package()` or ",
      "`usethis::use_dev_package()`.\n",
      sep = ""
    )
    out <- FALSE
  }
  if (isTRUE(arguments$allow_private_imports)) {
    return(out)
  }
  private <- setdiff(
    parse_data[c(parse_data$token[-1], FALSE) == "NS_GET_INT", ]$text,
    local_pkg_name
  )
  if (length(private) > 0) {
    cat(
      "You should generally not call any function with `:::` unless it's a function you ",
      "define yourself in this package. The following files have such function calls: ",
      paste0(file, collapse = ", "), "\n\n",
      "If you still think `:::` is a good idea, you can ",
      "allow private imports in your `.pre-commit-config.yaml` like this:

    -   id: deps-in-desc
        args: [--allow_private_imports]

      ",
      sep = ""
    )
    out <- FALSE
  }
  out
}
out <- lapply(arguments$files, deps_in_desc, arguments = arguments)
if (!all(unlist(out))) {
  stop("Dependency check failed", call. = FALSE)
}
