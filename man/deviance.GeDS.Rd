% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/S3methods.R, R/S3methods_GeDSboost-GeDSgam.R
\name{deviance.GeDS}
\alias{deviance.GeDS}
\alias{deviance.GeDSboost}
\alias{deviance.GeDSgam}
\title{Deviance Method for GeDS, GeDSgam, GeDSboost}
\usage{
\method{deviance}{GeDS}(object, n = 3L, ...)

\method{deviance}{GeDSgam}(object, n = 3L, ...)

\method{deviance}{GeDSboost}(object, n = 3L, ...)
}
\arguments{
\item{object}{The \code{"GeDS"}, \code{"GeDSgam"} or
\code{"GeDSboost"} class object from which the deviance should be extracted.}

\item{n}{Integer value (2, 3 or 4) specifying the order (\eqn{=} degree
\eqn{+ 1}) of the \code{"GeDS"}, \code{"GeDSgam"} or \code{"GeDSboost"} fit
whose deviance should be extracted. By default equal to \code{3L}; non-integer
values will be passed to the function \code{\link{as.integer}}.}

\item{...}{Potentially further arguments (required by the definition of the
generic function). These will be ignored, but with a warning.}
}
\value{
A numeric value corresponding to the  deviance of the selected
\code{"GeDS"}, \code{"GeDSgam"} or \code{"GeDSboost"} fit.
}
\description{
Method for the function \code{\link[stats]{deviance}} that allows the user to
extract the value of the deviance corresponding to a selected GeDS, GeDSboost
or GeDSgam fit typically returned by \code{\link{NGeDS}}/\code{\link{GGeDS}},
\code{\link{NGeDSgam}} or \code{\link{NGeDSboost}}.
}
\details{
This is a method for the function \code{\link[stats]{deviance}} in the
\pkg{stats} package. As \code{"GeDS"}, \code{"GeDSgam"} and \code{"GeDSboost"}
class objects contain three different fits (linear, quadratic and cubic), it
is possible to specify the order of the GeDS fit for which the deviance is
required via the input argument \code{n}.
}
\seealso{
\code{\link[stats]{deviance}} for the standard definition;
\code{\link{NGeDS}}, \code{\link{GGeDS}}, \code{\link{NGeDSgam}},
\code{\link{NGeDSboost}} for examples.
}
