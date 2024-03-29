% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nest.R
\name{nest_parse_data}
\alias{nest_parse_data}
\title{Nest a flat parse table}
\usage{
nest_parse_data(pd_flat)
}
\arguments{
\item{pd_flat}{A flat parse table including both terminals and non-terminals.}
}
\value{
A nested parse table.
}
\description{
\code{nest_parse_data} groups \code{pd_flat} into a parse table with tokens that are
a parent to other tokens (called internal) and such that are not (called
child). Then, the token in child are joined to their parents in internal
and all token information of the children is nested into a column "child".
This is done recursively until we are only left with a nested data frame that
contains one row: The nested parse table.
}
\seealso{
\code{\link[=compute_parse_data_nested]{compute_parse_data_nested()}}
}
\keyword{internal}
