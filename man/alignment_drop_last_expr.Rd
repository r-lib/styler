% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detect-alignment-utils.R
\name{alignment_drop_last_expr}
\alias{alignment_drop_last_expr}
\title{Remove last expression}
\usage{
alignment_drop_last_expr(pds_by_line)
}
\description{
In a \emph{nest}, if the last token is an \code{expr}, the \emph{nest} represents either
an if, while or for statement or a function call. We don't call about that
part, in fact it's important to remove it for alignment. See 'Examples'.
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
call(
  x = 12,
  y =  3,
)

function(a = 33,
         qq = 4) {
  # we don't care about this part for alignment detection
}
\dontshow{\}) # examplesIf}
}
\keyword{internal}
