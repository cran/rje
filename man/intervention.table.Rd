\name{intervention.table}
\alias{intervention.table}

\title{
Calculate interventional distributions.
}
\description{
Calculate interventional distributions from a probability table.
}
\usage{
intervention.table(x, variables, condition)
}
\arguments{
  \item{x}{
An array of probabilities.
}
  \item{variables}{
The margin for the intervention.
}
  \item{condition}{
The dimensions to be conditioned upon.
}
}
\details{
  This just divides the joint distribution \eqn{p(x)} by \eqn{p(v | c)}, where
  \eqn{v} is \code{variables} and \eqn{c} is \code{condition}.

  Under certain causal assumptions this is the interventional distribution
  \eqn{p(x \,|\, do(v))} (i.e. if the direct causes of \eqn{v} are precisely \eqn{c}.)
}
\value{
A numerical array of the same dimension as \eqn{x}.
}
\references{
Pearl, J., \emph{Causality}, 2nd Edition. Cambridge University Press, 2009.
}
\author{
Robin Evans
}

\seealso{
\code{\link{condition.table}}, 
\code{\link[base]{margin.table}}
}
\examples{
set.seed(413)
p = array(rdirichlet(1, rep(1,16)), rep(2,4))
intervention.table(p, 3, 2)
}
\keyword{arith}
