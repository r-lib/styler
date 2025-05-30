% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rules-indention.R
\name{unindent_function_declaration}
\alias{unindent_function_declaration}
\title{Revert the indention of function declaration header}
\usage{
unindent_function_declaration(pd, indent_by = 2L)
}
\arguments{
\item{pd}{A parse table.}

\item{indent_by}{How many spaces of indention should be inserted after
operators such as '('.}
}
\description{
Necessary for consistent indention of the function declaration header.
}
\seealso{
set_unindention_child update_indention_reference_function_declaration
}
\keyword{internal}
