\name{ghyp}
\alias{ghyp}
\alias{hyp}
\alias{NIG}            
\alias{student.t}
\alias{VG}
\title{Create generalized hyperbolic distribution objects}
\description{
  Constructor function for univariate and multivariate generalized hyperbolic objects 
  and its special cases.
}
\usage{
ghyp(lambda = 0.5, chi = 0.5, psi = 2, mu = 0, sigma = 1, gamma = 0, 
     alpha.bar = NULL, data = NULL)

hyp(chi = 0.5, psi = 2, mu = 0, sigma = 1, gamma = 0, alpha.bar = NULL, 
    data = NULL) 

NIG(chi = 2, psi = 2, mu = 0, sigma = 1, gamma = 0, alpha.bar = NULL, 
    data = NULL) 

student.t(nu = 5, mu = 0, sigma = 1, gamma = 0, data = NULL)  

VG(lambda = 1, psi = 2*lambda, mu = 0, sigma = 1, gamma = 0, data = NULL)
}
\arguments{
  \item{lambda}{Shape parameter.}
  \item{nu}{Shape parameter only used in case of a student-t distribution. It determines
            the degree of freedom and is defined as \code{-2*lambda}.}
  \item{chi}{Shape parameter of the alternative \dQuote{chi/psi} parametrization.}
  \item{psi}{Shape parameter of the alternative \dQuote{chi/psi} parametrization.}
  \item{alpha.bar}{Shape parameter of the alternative \dQuote{alpha.bar} parametrization.}
  \item{mu}{Location parameter. Either a scalar or a vector.}
  \item{sigma}{Dispersion parameter. Either a scalar or a matrix.}
  \item{gamma}{Skewness parameter. Either a scalar or a vector.}
  \item{data}{Can be of type \code{vector}, \code{matrix} or \code{data.frame}.}
}
\details{
  This function serves as a constructor for univariate and multivariate 
  generalized hyperbolic distribution objects and the special cases of the
  generalized hyperbolic distribution. \cr
  \code{ghyp} can be called either with the \dQuote{chi/psi} or the \dQuote{alpha.bar} 
  parametrization. 
  When ever \code{alpha.bar} is not \code{NULL} it is assumed that \dQuote{alpha.bar} 
  parameters were supplied.  \cr
  The parametrization of the \code{student.t} distribution slightly differs from the
  common student-t parametrization: The parameter \code{sigma} denotes the standard
  deviation. \cr
  Have a look on the vignette of this package in the \code{doc} folder.
}
\value{
 An object of class \code{\link[=ghypuv-class]{ghypuv}} or \code{\link[=ghypmv-class]{ghypmv}}.
}
\author{ David Lüthi }
\note{
  The \dQuote{alpha.bar} parametrization yields to a slightly different student-t parametrization:
  The parameter \code{sigma} denotes the variance in the multivariate case and
  the standard deviation in the univariate case. Thus, 
  set \code{sigma = sqrt(nu /(nu-2)} in the univariate case to get the same results as 
  with the standard \R implementation of the student-t distribution.\cr
  
  Once an object of class \code{\link[=ghypuv-class]{ghypuv}} or 
  \code{\link[=ghypmv-class]{ghypmv}} is created the methods
  \code{Xghyp} have to be used even when the distribution is a special case of
  the generalized hyperbolic distribution.  E.g. do not use \code{dVG}. 
  Use \code{\link{dghyp}} and submit a variance gamma
  distribution created with \code{VG}.  
}
\seealso{
  \code{\link{ghypuv-class}}, \code{\link{ghypmv-class}}, 
  \code{\link{fit.ghypuv}}, \code{\link{fit.ghypmv}}.
}
\examples{
  ## alpha.bar parametrization of a univariate generalized hyperbolic distribution
  ghyp(lambda=1, alpha.bar=0.1, mu=0, sigma=1, gamma=0)
  ## lambda/chi parametrization of a univariate generalized hyperbolic distribution
  ghyp(lambda=1, chi=1, psi=0.5, mu=0, sigma=1, gamma=0)
  
  ## alpha.bar parametrization of a multivariate generalized hyperbolic distribution
  ghyp(lambda=1, alpha.bar=0.1, mu=rep(0,2), sigma=diag(rep(1,2)), gamma=rep(0,2))
  ## lambda/chi parametrization of a multivariate generalized hyperbolic distribution
  ghyp(lambda=1, chi=1, psi=0.5, mu=rep(0,2), sigma=diag(rep(1,2)), gamma=rep(0,2))

  ## alpha.bar parametrization of a univariate hyperbolic distribution
  hyp(alpha.bar=0.3, mu=1, sigma=0.1, gamma=0)
  ## lambda/chi parametrization of a univariate hyperbolic distribution
  hyp(chi=1, psi=2, mu=1, sigma=0.1, gamma=0)

  ## alpha.bar parametrization of a univariate normal inverse gaussian distribution
  NIG(alpha.bar=0.3, mu=1, sigma=0.1, gamma=0)
  ## lambda/chi parametrization of a univariate normal inverse gaussian distribution
  NIG(chi=1, psi=2, mu=1, sigma=0.1, gamma=0)
  
  ## alpha.bar parametrization of a univariate variance gamma distribution   
  VG(lambda=2, mu=1, sigma=0.1, gamma=0)
  
  ## alpha.bar parametrization of a univariate student-t distribution 
  student.t(nu = 3, mu=1, sigma=0.1, gamma=0)
}
\keyword{distribution}
\keyword{multivariate}
\keyword{models}