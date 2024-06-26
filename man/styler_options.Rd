% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/options.R
\name{styler_options}
\alias{styler_options}
\title{Package options}
\description{
These options can be set via \code{\link[=options]{options()}} and queried via \code{\link[=getOption]{getOption()}}.
For this, add a \code{styler.} prefix (the package name and a dot) to the option name.
Example: for an option \code{foo}, use \code{options(styler.foo = value)} to set it
and \code{getOption("styler.foo")} to retrieve the current value.
An option value of \code{NULL} means that the default is used.
}
\section{Options for the styler package}{

\itemize{
\item \code{styler.addins_style_transformer}: character.
The name of the style transformer to use in the addins.
(default: \code{"styler::tidyverse_style()"})
\item \code{styler.cache_name}: character.
The name of the styler cache to use.
(default: 1.10.3.9000)
\item \code{styler.cache_root}: character.
The directory where the cache files are stored. For more, see \code{help("caching")}.
(default: \code{NULL})
\item \code{styler.colored_print.vertical}: logical.
It decides whether or not the output should be colored with \code{prettycode::highlight()}.
(default: \code{TRUE})
\item \code{styler.ignore_alignment}: logical.
If \code{TRUE}, alignment, when detected, is ignored. For more, see \code{vignette("detect-alignment")}.
(default: \code{FALSE})
\item \code{styler.ignore_start}, \code{styler.ignore_stop}: character.
Regular expressions to ignore lines that match them. For more, see \code{help("stylerignore")}.
(default: \code{"styler: off"} and \code{"styler: on"}, respectively)
\item \code{styler.quiet}: logical.
It decides whether or not to print an informative message about what the function is doing.
If \code{TRUE}, no output is printed.
(default: \code{FALSE})
\item \code{styler.test_dir_writable}: logical.
If \code{TRUE}, the package tests whether the directory is writable.
(default: \code{TRUE})
}
}

\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
getOption("styler.ignore_alignment")
options(
  styler.ignore_alignment = TRUE,
  styler.quiet = TRUE
)
getOption("styler.ignore_alignment")
\dontshow{\}) # examplesIf}
}
