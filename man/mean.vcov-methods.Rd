\name{Expected value and variance}
\alias{mean.ghypbase}
\alias{mean-methods}
\alias{mean,ghypbase-method}
\alias{vcov.ghypbase}
\alias{vcov-methods}
\alias{vcov,ghypbase-method}
\title{Expected value and variance-covariance of generalized hyperbolic distributions}
\description{
  The function \code{mean} returns the expected value. The function \code{vcov} returns
  the variance in the univariate case and the variance covariance matrix in the
  multivariate case.
}
\usage{
\S4method{mean}{ghypbase}(x)
\S4method{vcov}{ghypbase}(object)
}
\arguments{
  \item{x, object}{An object inheriting from class 
                   \code{\link[=ghypbase-class]{ghypbase}}.}
}
\value{
  Either the expected value or the variance.
}
\author{David Lüthi}
\seealso{\code{\link{ghyp}}, \code{\link{ghypbase-class}}, \code{\link{ghyp.moments}}}
\examples{
  ## Univariate: Parametric 
  vg.dist <- VG(lambda=1.10,mu=10,sigma=10,gamma=2)
  mean(vg.dist)
  vcov(vg.dist)
  ## Univariate: Empirical                                                 
  vg.sim <- rghyp(10000,vg.dist)
  mean(vg.sim)
  var(vg.sim)

  ## Multivariate: Parametric 
  vg.dist <- VG(lambda=.10,mu=c(55,33),sigma=diag(c(22,888)),gamma=1:2)
  mean(vg.dist)
  vcov(vg.dist)
  ## Multivariate: Empirical                                                 
  vg.sim <- rghyp(10000,vg.dist)
  colMeans(vg.sim)
  var(vg.sim)  
}
\keyword{distribution}
\keyword{models}
\keyword{hplot}
\keyword{methods}
