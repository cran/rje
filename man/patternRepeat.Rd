\name{patternRepeat}
\alias{patternRepeat}
\title{
Complex repetitions
}
\description{
Recreate patters for collapsed arrays
}
\usage{
patternRepeat(x, which, n, careful = TRUE)
}
\arguments{
  \item{x}{
A vector to be repeated
}
  \item{which}{
Which indices of the implicit array are given in \code{x}
}
  \item{n}{
Dimensions of implicit array.
}
\item{careful}{
logical indicating whether to check vailidty of other arguments or work
quickly (mainly for recursion)
  }
}
\details{
This function allows for complex repeating patterns.  Consider an array with dimensions \code{n}; then for each value of the dimensions in \code{which} (contained in \code{x}), this function returns the pattern of values in \code{which} for each entry in the larger array.

Hence the length of \code{x} must be equal to \code{prod(n[which])}.
}
\value{
A vector of length \code{prod(n)} containing suitably repeated and ordered elements of \code{x}.
}
\author{
Robin Evans
}

\seealso{
\code{\link{rep}}
}
\examples{
patternRepeat(1:4, c(1,2), c(2,2,2))
c(array(1:4, c(2,2,2)))

patternRepeat(1:4, c(1,3), c(2,2,2))
patternRepeat(1:4, c(2,3), c(2,2,2))
}
\keyword{array}
