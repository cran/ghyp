\name{Log-Likelihood and Akaike's Information Criterion}
\alias{logLik.mle.ghyp}
\alias{logLik,mle.ghypmv-method}
\alias{logLik,mle.ghypuv-method}
\alias{AIC.mle.ghyp}
\alias{AIC,mle.ghypmv-method}
\alias{AIC,mle.ghypuv-method}
\title{Extract Log-Likelihood and Akaike's Information Criterion}
\description{
  The functions \code{logLik} and \code{AIC} 
  extract the Log-Likelihood and the Akaike's Information Criterion from fitted
  generalized hyperbolic distribution objects.
}
\usage{
\S4method{logLik}{mle.ghypmv}(object, \dots)
\S4method{logLik}{mle.ghypuv}(object, \dots)
\S4method{AIC}{mle.ghypuv}(object, ..., k = 2)
\S4method{AIC}{mle.ghypmv}(object, ..., k = 2)
}
\arguments{
  \item{object}{Either an object of class
                   \code{\link[=mle.ghypuv-class]{mle.ghypuv}} or
                   \code{\link[=mle.ghypmv-class]{mle.ghypmv}}.}
  \item{k}{The \dQuote{penalty} per parameter to be used; the default k = 2 is the classical AIC.} 
  \item{\dots}{An arbitrary number of objects of classes
                   \code{\link[=mle.ghypuv-class]{mle.ghypuv}} or
                   \code{\link[=mle.ghypmv-class]{mle.ghypmv}}.}                   
}
\value{
  Either the Log-Likelihood or the Akaike's Information Criterion.
}
\author{David Lüthi}
\note{
The Log-Likelihood as well as the Akaike's Information Criterion can be obtained from
the function \code{\link{ghyp.fit.info}}. However, the benefit of \code{logLik} and \code{AIC}
is that these functions allow a call with an arbitrary number of objects and are better known
because they are generic.
}
\seealso{\code{\link{fit.ghypuv}}, \code{\link{fit.ghypmv}}, 
         \code{\link{ghyp.fit.info}}, \code{\link{mle.ghypuv-class}}, \code{\link{mle.ghypmv-class}}}
\examples{
  data(smi.stocks)
  
  ## Multivariate fit
  fit.mv <- fit.hypmv(smi.stocks,nit=10)
  AIC(fit.mv)
  logLik(fit.mv)
  
  ## Univariate fit
  fit.uv <- fit.tuv(smi.stocks[,"CS"],control=list(maxit=10))
  AIC(fit.uv)
  logLik(fit.uv) 
}
\keyword{methods}
