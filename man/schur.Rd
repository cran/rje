% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schur.R
\name{schur}
\alias{schur}
\title{Obtain generalized Schur complement}
\usage{
schur(M, x, y, z)
}
\arguments{
\item{M}{symmetric positive definite matrix}

\item{x, y, z}{indices of M to calculate with (see below)}
}
\description{
Obtain generalized Schur complement
}
\details{
Calculates \eqn{M_{xy} - M_{xz} M^{zz} M_{zy}}, which
(if M is a Gaussian covariance matrix) is the covariance between
x and y after conditioning on z.

y defaults to equal x, and z to be the complement of \eqn{x \cup y}.
}
