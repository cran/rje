\name{printPercentage}
\alias{printPercentage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print Percentage of Activity Completed to stdout
}
\description{
Used to print percentages of loop or similar process which has been completed %
to the user.
}
\usage{
printPercentage(i, n, dp = 0, first = 1, last = n, prev = i - 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{the number of iterations completed.}
  \item{n}{total number of iterations.}
  \item{dp}{number of decimal places to display.}
  \item{first}{number of the first iteration for which this percentage was %
    displayed}
  \item{last}{number of the final iteration for which this percentage will be %
    displayed}
  \item{prev}{number of the previous iteration for which this percentage was %
    displayed}
}
\details{
Function will use \code{cat} to print the proportion of loops which have been %
completed (i.e. \code{i/n}) to the standard output.  In doing so it will erase %
the previous such percentage, except when \code{i = first}.  A new line is %
added when \code{i = last}, assuming that the loop is finished.
}
\value{
\code{NULL}
}
\author{
Robin Evans
}

\section{Warning}{
This will fail to work nicely if other test is printed to the standard output %
during the process.
}

\examples{
x = numeric(100)

for (i in 1:100) {
  x[i] = mean(rnorm(1e5))
  printPercentage(i,100)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{iteration}
\keyword{IO}
\keyword{print}
