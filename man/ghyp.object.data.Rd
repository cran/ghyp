\name{ghyp object data}
\alias{ghyp.params}
\alias{ghyp.data}
\alias{ghyp.fit.info}
\title{Get methods for objects inheriting from class ghypbase}
\description{
  These functions simply return values stored within generalized hyperbolic
  distribution objects. 
  \code{ghyp.fit.info} extract information about the fitting procedure from objects of 
  class  \code{\link[=mle.ghypuv-class]{mle.ghypuv}} and
  \code{\link[=mle.ghypmv-class]{mle.ghypmv}}.
}
\usage{
ghyp.params(object, type = c("chipsi", "alpha.bar"))

ghyp.fit.info(object)

ghyp.data(object)
}
\arguments{
  \item{object}{An object inheriting from class 
               \code{\link[=ghypbase-class]{ghypbase}}.}
  \item{type}{The parameters of either the (\code{lambda, alpha.bar, mu, sigma, gamma}) or
              the (\code{lambda, chi, psi, mu, sigma, gamma})
              parametrization will be returned.}
}
\value{
  \code{ghyp.params} reutrns the parameters of the generalized hyperbolic distribution                     
                     as a list with components:
  \tabular{rl}{
  \code{lambda} \tab Shape parameter. \cr
  \code{alpha.bar} \tab Shape parameter. \cr
  \code{chi/psi} \tab Shape parameters. \cr  
  \code{mu} \tab Location parameter. \cr
  \code{sigma}\tab Dispersion parameter.\cr   
  \code{gamma}\tab Skewness parameter.\cr   
  }
  
  \code{ghyp.fit.info} returns list with components: 
  \tabular{rl}{
  \code{logLikelihood} \tab  The maximized log-likelihood value.\cr
  \code{aic} \tab  The Akaike information criterion.\cr
  \code{fitted.params} \tab  A boolean vector stating which parameters were fitted.\cr
  \code{converged} \tab  A boolean whether \code{\link{optim}} converged or not.\cr
  \code{n.iter} \tab The number of iterations. \cr
  \code{error.code} \tab Error code from \code{\link{optim}}.\cr
  \code{error.message} \tab Error message from \code{\link{optim}}.\cr  
  \code{parameter.variance} \tab Parameter variance (only for univariate fits).\cr
  }

  \code{ghyp.data} returns \code{NULL} if no data is stored within the
  object, a \code{vector} if the object is of class 
  \code{\link[=mle.ghypuv-class]{mle.ghypuv}} or a \code{matrix} if the object
  is of class \code{\link[=mle.ghypmv-class]{mle.ghypmv}}.
}
\author{David Lüthi}
\note{
  \code{ghyp.fit.info} requires an object of 
  class \code{\link[=mle.ghypuv-class]{mle.ghypuv}} or
  \code{\link[=mle.ghypmv-class]{mle.ghypmv}}. In the univariate case the parameter
  variance is returned as well. The parameter variance is defined as the inverse of the
  negative hesse-matrix computed by \code{\link{optim}}. Note that this makes sense only
  in the case that the estimates are asymptotically normal distributed. \cr
  
  The class \code{\link[=ghypbase-class]{ghypbase}} contains a \code{data} slot.
  Data can be stored either when initializing an object of class 
  \code{\link[=ghypuv-class]{ghypuv}} or 
  \code{\link[=ghypmv-class]{ghypmv}} by using \code{\link{ghyp}} or
  via the fitting routines  \code{\link{fit.ghypuv}} or \code{\link{fit.ghypmv}}.
}
\seealso{\code{\link[=mean.ghypbase]{mean}}, \code{\link[=vcov.ghypbase]{vcov}},
         \code{\link[=logLik.mle.ghyp]{logLik}}, \code{\link[=AIC.mle.ghyp]{AIC}},   
         \code{\link{fit.ghypmv}}, \code{\link{fit.ghypuv}}, \code{\link{ghyp}}, \code{\link{optim}}}
\examples{
  ## multivariate generalized hyperbolic distribution
  ghyp.mv <- ghyp(lambda=1, alpha.bar=0.1, mu=rep(0,2), sigma=diag(rep(1,2)), 
                  gamma=rep(0,2),data=matrix(rt(1000,df=4),ncol=2))
  ## Get parameters
  ghyp.params(ghyp.mv,type="alpha.bar")
  ghyp.params(ghyp.mv,type="chipsi")  

  ## Get data
  ghyp.data(ghyp.mv)

  ## 'ghyp.fit.info' does only work when the object is of class 'mle.ghypuv' 
  ## or 'mle.ghypmv', i.e. is created using 'fit.ghypuv' etc.
  mv.fit <- fit.tmv(data = ghyp.data(ghyp.mv), control=list(abs.tol=1e-3))
  ghyp.fit.info(mv.fit)
}
\keyword{utilities}
