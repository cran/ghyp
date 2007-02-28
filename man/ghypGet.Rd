\name{ghyp get methods}
\alias{ghyp.moments}
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

ghyp.moments(object)

ghyp.data(object)

ghyp.fit.info(object)
}
\arguments{
  \item{object}{An object inheriting from class 
               \code{\link[=ghypbase-class]{ghypbase}}.}
  \item{type}{The parameters of either the \dQuote{alpha.bar} or the \dQuote{chi/psi}
                    parametrization will be returned.}
}
\value{
  \code{ghyp.params} reutrns a list with components:
  \tabular{rl}{
  \code{e.ghyp} \tab Expected value of a generalized hyperbolic distribution. \cr
  \code{var.ghyp} \tab Variance of a generalized hyperbolic distribution.\cr
  \code{e.gig} \tab Expected value of the corresponding generalized inverse gaussian distribution.\cr
  \code{var.gig}\tab Variance of the corresponding generalized inverse gaussian distribution.\cr   
  }
  
  \code{ghyp.moments} reutrns a list with components:  
  \tabular{rl}{
  \code{e.ghyp} \tab Expected value of a generalized hyperbolic distribution.\cr
  \code{var.ghyp} \tab Variance of a generalized hyperbolic distribution.\cr
  \code{e.gig} \tab Expected value of the corresponding generalized inverse gaussian distribution.\cr
  \code{var.gig} \tab Variance of the corresponding generalized inverse gaussian distribution.\cr  
  }

  \code{ghyp.fit.info} returns list with components: 
  \tabular{rl}{
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
  \code{e.gig} and \code{var.gig} have to be interpreted as
  the expected value and variance of the gamma or inverse
  gamma distribution 
  when the distribution is a special case of the generalized
  hyperbolic distribution.\cr
  
  \code{ghyp.fit.info} requires an object of 
  class \code{\link[=mle.ghypuv-class]{mle.ghypuv}} or
  \code{\link[=mle.ghypmv-class]{mle.ghypmv}}. In the univariate case the parameter
  variance is returned as well. The parameter variance is defined as the inverse of the
  negative hesse-matrix computed by \code{\link{optim}}. Note that this makes sense only
  in the case that the estimates are asymptotically normal distributed. \cr
  
  The class \code{\link[=ghypbase-class]{ghypbase}} contains a \code{data} slot.
  Data can be stored either when initializing an object of class 
  \code{\link[=ghypuv-class]{ghypuv}} or 
  \code{\link[=ghypmv-class]{ghypmv}} or using \code{\link{ghyp}} or
  via the fitting routines  \code{\link{fit.ghypuv}} or \code{\link{fit.ghypmv}}.\cr
  
  When showing special cases of the generalized hyperbolic distribution
  the corresponding parameters are plotted in brackets.
}
\seealso{\code{\link[=mean.ghypbase]{mean}}, \code{\link[=vcov.ghypbase]{vcov}}, 
         \code{\link{fit.ghypmv}}, \code{\link{fit.ghypuv}}, \code{\link{ghyp}}, \code{\link{optim}}}
\examples{
  ## multivariate generalized hyperbolic distribution
  ghyp.mv <- ghyp(lambda=1, alpha.bar=0.1, mu=rep(0,2), sigma=diag(rep(1,2)), 
                  gamma=rep(0,2),data=matrix(rt(1000,df=4),ncol=2))
  ## Get parameters
  ghyp.params(ghyp.mv,type="alpha.bar")
  ghyp.params(ghyp.mv,type="chipsi")  

  ## Get moments
  ghyp.moments(ghyp.mv)

  ## Get data
  ghyp.data(ghyp.mv)

  ## 'ghyp.fit.info' does only work when the object is of class 'mle.ghypuv' 
  ## or 'mle.ghypmv', i.e. is created using 'fit.ghypuv' etc.
  mv.fit <- fit.tmv(data = ghyp.data(ghyp.mv))
  ghyp.fit.info(mv.fit)
}
\keyword{internal}
\keyword{utilities}
