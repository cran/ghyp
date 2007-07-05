\name{ghyp moment computation}
\alias{ghyp.moment}
\title{Compute moments of ghyp distributions}
\description{
  This function computes moments of arbitrary orders of the univariate generalized
  hyperbolic distribution. 
}
\usage{
ghyp.moment(object, order = 3:4, absolute = FALSE, central = TRUE, ...)
}
\arguments{
  \item{object}{An object inheriting from class 
               \code{\link[=ghypuv-class]{ghypuv}}.}
  \item{order}{A vector containing the order of the moments.}
  \item{absolute}{Indicate whether the absolute value is taken or not. Must be \code{TRUE}
                  if \code{order} is not integer.}
  \item{central}{If \code{TRUE} the moment about the expected value is computed.}
  \item{\dots}{Arguments passed to \code{\link{integrate}}.}  
}
\value{
  A vector which contains the moments.
}
\author{David Lüthi}
\seealso{\code{\link[=mean.ghypbase]{mean}}, \code{\link[=vcov.ghypbase]{vcov}},
         \code{\link{Egig}}}
\examples{
  nig.uv <- NIG(alpha.bar=0.1, mu=1.1, sigma=3, gamma=-2)
  # Moments of integer order
  ghyp.moment(nig.uv, order = 1:6)
  # Moments of fractional order  
  ghyp.moment(nig.uv, order = 0.2 * 1:20, absolute = TRUE)
}
\keyword{utilities}
