% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3methods.R
\name{knots.GeDS}
\alias{knots.GeDS}
\title{Knots method for GeDS objects}
\usage{
\method{knots}{GeDS}(Fn, n = 3L, options = c("all", "internal"), ...)
}
\arguments{
\item{Fn}{the \code{\link{GeDS-class}} object from which the vector of knots for the specified GeDS fit
should be extracted.}

\item{n}{integer value (2, 3 or 4) specifying the order (\eqn{=} degree \eqn{+ 1}) of the GeDS fit
whose knots should be extracted.
By default equal to \code{3L}.
Non-integer values will be passed to the function \code{\link{as.integer}}.}

\item{options}{a character string specifying whether "\code{all}" knots, including
the left-most and the right-most limits of the interval embedding the observations (the default) or
only the "\code{internal}" knots should be extracted.}

\item{...}{potentially further arguments (required for compatibility with the definition of
the generic function). Currently ignored, but with a warning.}
}
\value{
A vector in which each element represents a knot of the GeDS fit of the required order.
}
\description{
Method for the generic function \code{\link[stats]{knots}} that allows the user
to extract vector of knots of a GeDS fit of a specified order
contained in a \code{\link{GeDS-class}} object.
}
\details{
This is a method for the function \code{\link[stats]{knots}} in the \pkg{stats} package.

As \code{\link{GeDS-class}} objects contain three different fits (linear, quadratic and cubic), it is possible
to specify the order of the GeDS fit  whose knots  are required via the input argument \code{n}.
}
\seealso{
\code{\link[stats]{knots}} for the definition of the generic function; \code{\link{NGeDS}} and \code{\link{GGeDS}} for examples.
}

