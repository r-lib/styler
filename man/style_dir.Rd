% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ui-styling.R
\name{style_dir}
\alias{style_dir}
\title{Prettify arbitrary R code}
\usage{
style_dir(
  path = ".",
  ...,
  style = tidyverse_style,
  transformers = style(...),
  filetype = c("R", "Rprofile", "Rmd", "Rmarkdown", "Rnw", "Qmd"),
  recursive = TRUE,
  exclude_files = NULL,
  exclude_dirs = c("packrat", "renv"),
  include_roxygen_examples = TRUE,
  base_indention = 0L,
  dry = "off"
)
}
\arguments{
\item{path}{Path to a directory with files to transform.}

\item{...}{Arguments passed on to the \code{style} function,
see \code{\link[=tidyverse_style]{tidyverse_style()}} for the default argument.}

\item{style}{A function that creates a style guide to use, by default
\code{\link{tidyverse_style}}. Not used
further except to construct the argument \code{transformers}. See
\code{\link[=style_guides]{style_guides()}} for details.}

\item{transformers}{A set of transformer functions. This argument is most
conveniently constructed via the \code{style} argument and \code{...}. See
'Examples'.}

\item{filetype}{Vector of file extensions indicating which file types should
be styled. Case is ignored, and the \code{.} is optional, e.g. \code{c(".R",".Rmd")},
or \code{c("r", "rmd")}. Supported values (after standardization) are:
"qmd", "r", "rmd", "rmarkdown", "rnw", and "rprofile".
Rmarkdown is treated as Rmd.}

\item{recursive}{A logical value indicating whether or not files in
sub directories of \code{path} should be styled as well.}

\item{exclude_files}{Character vector with regular expressions to files
that should be excluded from styling.}

\item{exclude_dirs}{Character vector with directories to exclude
(recursively).}

\item{include_roxygen_examples}{Whether or not to style code in roxygen
examples.}

\item{base_indention}{Integer scalar indicating by how many spaces the whole
output text should be indented. Note that this is not the same as splitting
by line and add a \code{base_indention} spaces before the code in the case
multi-line strings are present. See 'Examples'.}

\item{dry}{To indicate whether styler should run in \emph{dry} mode, i.e. refrain
from writing back to files .\code{"on"} and \code{"fail"} both don't write back, the
latter returns an error if the input code is not identical to the result
of styling. "off", the default, writes back if the input and output of
styling are not identical.}
}
\description{
Performs various substitutions in all \code{.R}, \code{.Rmd}, \code{.Rmarkdown}, \code{qmd}
and/or \code{.Rnw} files in a directory (by default only \code{.R} files are styled -
see \code{filetype} argument).
Carefully examine the results after running this function!
}
\section{Value}{

Invisibly returns a data frame that indicates for each file considered for
styling whether or not it was actually changed (or would be changed when
\code{dry} is not "off").
}

\section{Warning}{

This function overwrites files (if styling results in a change of the
code to be formatted and \code{dry = "off"}). It is strongly suggested to only
style files that are under version control or to create a backup copy.

We suggest to first style with \code{scope < "tokens"} and inspect and commit
changes, because these changes are guaranteed to leave the abstract syntax
tree (AST) unchanged. See section 'Round trip validation' for details.

Then, we suggest to style with \code{scope = "tokens"} (if desired) and carefully
inspect the changes to make sure the AST is not changed in an unexpected way
that invalidates code.
}

\section{Round trip validation}{

The following section describes when and how styling is guaranteed to
yield correct code.

If tokens are not in the styling scope (as specified with the \code{scope}
argument), no tokens are changed and the abstract syntax tree (AST) should
not change.
Hence, it is possible to validate the styling by comparing whether the parsed
expression before and after styling have the same AST.
This comparison omits roxygen code examples and comments. styler throws an
error if the AST has changed through styling.

Note that if tokens are to be styled, such a comparison is not conducted because
the AST might well change and such a change is intended. There is no way
styler can validate styling, that is why we inform the user to carefully
inspect the changes.

See section 'Warning' for a good strategy to apply styling safely.
}

\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
style_dir("path/to/dir", filetype = c("rmd", ".R"))

# the following is identical (because of ... and defaults)
# but the first is most convenient:
style_dir(strict = TRUE)
style_dir(style = tidyverse_style, strict = TRUE)
style_dir(transformers = tidyverse_style(strict = TRUE))
\dontshow{\}) # examplesIf}
}
\seealso{
Other stylers: 
\code{\link{style_file}()},
\code{\link{style_pkg}()},
\code{\link{style_text}()},
\code{\link{styler_addins}}
}
\concept{stylers}
