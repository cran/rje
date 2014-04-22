\name{marginTable}
\alias{marginTable}
\alias{propTable}

\title{
  Compute table margin faster
}
\description{
  For a contingency table in array form, compute the sum of table
  entries for a given index.
}
\usage{
marginTable(x, margin = NULL, order = TRUE)
propTable(x, margin = NULL)
}
\arguments{
  \item{x}{
    a numeric array
}
  \item{margin}{
    vector of index numbers (1 for rows, etc.)
}
  \item{order}{
    logical indicating whether indices of output should be ordered as in the
    vector \code{margin}?
}
}
\details{
  With \code{order = TRUE} this is the same as the base function
  \code{margin.table()}, but faster.

  With \code{order = FALSE} the function is even faster, but the indices
  in the margin are returned in their original order, regardless of the
  way they are specified in \code{margin}.

  \code{propTable()} is equivalent to \code{prop.table()}, but faster.
}
\value{
  The relevant marginal table.  The class of 'x' is copied to the
  output table, except in the summation case.
}
\author{
Robin Evans
}
\note{
  Original functions are \code{\link[base]{margin.table}} and
  \code{\link[base]{prop.table}}.
}

\examples{
m <- matrix(1:4, 2)
marginTable(m, 1)
marginTable(m, 2)

propTable(m, 2)
}
\keyword{array}
