\name{condition.table}
\alias{condition.table}
\title{
Find conditional probability table
}
\description{
Given a numeric array, calculates margins of some dimensions conditional on particular values of others.
}
\usage{
condition.table(x, variables, condition = NULL, condition.value = NULL)
}
\arguments{
  \item{x}{
A numeric array.
}
  \item{variables}{
An integer vector containing the margins of \code{x} interest.
}
  \item{condition}{
An integer vector containing the dimensions of \code{x} to condition on.
}
  \item{condition.value}{
An integer vector or list of the same length as \code{condition}, containing the values to condition with.
}
}
\details{
Calculates the marginal distribution over the dimensions in \code{variables} for each specified value of the dimensions in \code{condition}.  Single or multiple values of each dimension in \code{condition} may be specified in \code{condition.value}; in the case of multiple values, \code{condition.value} must be a list.

The sum over the dimensions in \code{variables} is normalized to 1 for each value of \code{condition}.
}
\value{
An array whose first \code{length(variables)} corresponds to the dimensions in \code{variables}, and the remainder (if any) to dimensions in \code{condition} with a corresponding entry in \code{condition.value} of length > 1.
}
\author{
Mathias Drton, Robin Evans
}

\seealso{
\code{\link{margin.table}}
}
\examples{
x = array(1:16, rep(2,4))
x = x/sum(x) # probability distribution on 4 binary variables x1, x2, x3, x4.

# distribution of x2, x3 given x1 = 1 and x4=2.
condition.table(x, c(2,3), c(1,4), c(1,2))
# x2, x3 given x1 = 1,2 and x4 = 2.
condition.table(x, c(2,3), c(1,4), list(1:2,2))
}

\keyword{array}
