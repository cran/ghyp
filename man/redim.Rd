\name{redim}
\alias{redim}
\title{Extract dimensions of multivariate generalized hyperbolic distributions}
\description{
  This function may be useful when certain dimensions of a 
  multivariate generalized hyperbolic distribution are requested. 
  A generalized hyperbolic distribution object will be returned.
}
\usage{
redim(object, dim = c(1, 2))
}
\arguments{
  \item{object}{An object of class \code{\link[=ghypmv-class]{ghypmv}}.}
  \item{dim}{An integer \code{vector} with dimensions.}
}
\value{
  An object of class \code{\link[=ghypuv-class]{ghypuv}} or 
  \code{\link[=ghypmv-class]{ghypmv}}.
}
\author{David Lüthi}
\seealso{\code{\link{fit.ghypmv}}, \code{\link{lin.transf}}}
\examples{
  ## Mutivariate generalized hyperbolic distribution
  multivariate.ghyp <- ghyp(sigma=var(matrix(rnorm(9),ncol=3)),mu=1:3,gamma=-(2:0))
  redim(multivariate.ghyp,dim=1)
  redim(multivariate.ghyp,dim=c(1,3))
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
