\name{coef-methods}
\alias{coef.ghyp}
\alias{coef,ghyp-method}
\alias{coefficients,ghyp-method}
\title{Extract parameters of generalized hyperbolic distribution objects}
\description{
  The function \code{coef} reutrns the parameters of a generalized hyperbolic distribution 
  object as a list. The user can choose between the \dQuote{chi/psi} and the \dQuote{alpha.bar}
  parametrization. The function \code{coefficients} is a synonym for \code{coef}.
}
\usage{
\S4method{coef}{ghyp}(object, type = c("chipsi", "alpha.bar"))

\S4method{coefficients}{ghyp}(object, type = c("chipsi", "alpha.bar"))
}
\arguments{
  \item{object}{An object inheriting from class \code{\link[=ghyp-class]{ghyp}}.}
  \item{type}{According to \code{type} the parameters of either the \dQuote{chi/psi} or the \dQuote{alpha.bar}
              parametrization will be returned.} 
}
\value{
  If \code{type} is \dQuote{chipsi} a list with components:
  \tabular{rl}{
  \code{lambda} \tab Shape parameter. \cr
  \code{chi} \tab Shape parameter. \cr
  \code{psi} \tab Shape parameters. \cr  
  \code{mu} \tab Location parameter. \cr
  \code{sigma}\tab Dispersion parameter.\cr   
  \code{gamma}\tab Skewness parameter.\cr   
  }
  
  If \code{type} is \dQuote{alpha.bar} a list with components:
  \tabular{rl}{
  \code{lambda} \tab Shape parameter. \cr
  \code{alpha.bar} \tab Shape parameter. \cr
  \code{mu} \tab Location parameter. \cr
  \code{sigma}\tab Dispersion parameter.\cr   
  \code{gamma}\tab Skewness parameter.\cr   
  }
}
\author{David Lüthi}
\seealso{\code{\link{fit.ghypuv}}, \code{\link{fit.ghypmv}}, 
         \code{\link{ghyp.fit.info}}, \code{\link[=transform.ghyp]{transform}}, \code{\link{[.ghyp}}}
\examples{
  ghyp.mv <- ghyp(lambda=1, alpha.bar=0.1, mu=rep(0,2), sigma=diag(rep(1,2)), 
                  gamma=rep(0,2),data=matrix(rt(1000,df=4),ncol=2))
  ## Get parameters
  coef(ghyp.mv, type = "alpha.bar")
  coefficients(ghyp.mv, type = "chipsi") 
  
  ## Simple modification (do not modify slots directly e.g. object@mu <- 0:1)
  param <- coef(ghyp.mv, type = "alpha.bar")
  param$mu <- 0:1
  do.call("ghyp", param) # returns a new 'ghyp' object
  
  
}
\keyword{methods}
\keyword{utilities}