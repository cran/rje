% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsapply.R
\name{fsapply}
\alias{fsapply}
\title{Fast and loose application of function over list.}
\usage{
fsapply(x, FUN)
}
\arguments{
\item{x}{a vector (atomic or list) or an expression object.}

\item{FUN}{the function to be applied to each element of \code{x}. In the
case of functions like \code{+}, the function name must be backquoted or
quoted.}
}
\value{
A vector of results of applying \code{FUN} to \code{x}.
}
\description{
Faster highly stripped down version of \code{sapply()}
}
\details{
This is just a wrapper for \code{unlist(lapply(x, FUN))}, which will behave
as \code{sapply} if \code{FUN} returns an atomic vector of length 1 each
time.

Speed up over sapply is not dramatic, but can be useful in time critical
code.
}
\section{Warning }{
 Very loose version of \code{sapply} which should really
only by used if you're confident about how \code{FUN} is applied to each
entry in \code{x}.
}

\examples{

x = list(1:1000)
tmp = fsapply(x, sin)

\dontrun{
x = list()
set.seed(142313)
for (i in 1:1000) x[[i]] = rnorm(100)

system.time(for (i in 1:100) sapply(x, function(x) last(x)))
system.time(for (i in 1:100) fsapply(x, function(x) last(x)))
}

}
\author{
Robin Evans
}
\keyword{list}
