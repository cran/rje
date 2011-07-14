\name{combinations}
\alias{combinations}
\alias{powerSetMat}
\title{
Combinations of Integers
}
\description{
Returns a matrix containing each possible combination of one entry from vectors 
of the lengths provided.
}
\usage{
combinations(p)
powerSetMat(n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
 vector of non-negative integers.
}
  \item{n}{
 non-negative integer.
}
}
\details{
Returns a matrix, each row being one possible combination of integers from the
vectors \eqn{0, 1, \ldots, p_i-1}{0, 1,..., p_i - 1}.

Based on \code{bincombinations} from package \code{e1071}, which provides the binary case.

\code{powerSetMat} is just a wrapper for \code{combinations(rep(2, n))}.
}
\value{
A matrix with number of columns equal to the length of \code{p}, and number of
rows equal to \eqn{p_1 \times \cdots \times p_k}{p_1 * ... * p_k}, each row
corresponding to a different combination.  Ordering is lexographic.
}
\author{
Robin Evans
}

\examples{
combinations(c(2,3,3))

powerSetMat(3)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}
