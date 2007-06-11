\name{lin.transf}
\alias{lin.transf}
\title{Linear transformation of generalized hyperbolic distributed random numbers}
\description{
This function may be useful when generalized hyperbolic distribution objects
should be linearly transformed. A generalized hyperbolic distribution object will
be returned.
}
\usage{
lin.transf(object, summand, multiplier)
}
\arguments{
  \item{object}{An object inheriting from class \code{\link[=ghypbase-class]{ghypbase}}.}
  \item{summand}{A \code{vector}.}
  \item{multiplier}{A vector or a matrix.}
}
\value{
  An object of class \code{\link[=ghypuv-class]{ghypuv}} or 
  \code{\link[=ghypmv-class]{ghypmv}}.
}
\author{David Lüthi}
\seealso{\code{\link{redim}}, \code{\link{ghyp}}}
\examples{
  ## Mutivariate generalized hyperbolic distribution
  multivariate.ghyp <- ghyp(sigma=var(matrix(rnorm(9),ncol=3)),mu=1:3,gamma=-2:0)
  ## Dimension reduces to 2
  lin.transf(multivariate.ghyp,multiplier=matrix(1:6,nrow=2),summand=10:11)
  ## Dimension reduces to 1
  lin.transf(multivariate.ghyp, multiplier=1:3)
  ## Simple transformation
  lin.transf(multivariate.ghyp, summand=100:102)
}
\keyword{utilities}
     