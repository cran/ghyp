\name{mle.ghypuv-class}
\docType{class}
\alias{mle.ghypuv-class}
\alias{show.mle.ghypuv}
\alias{show,mle.ghypuv-method}
\title{Class mle.ghypuv}
\description{The class \dQuote{mle.ghypuv} inherits from the class \dQuote{ghypuv}. In addition
  to the class \dQuote{ghypuv} this class stores fitting information. Namely the
  number of iterations \code{n.iter}, the log likelihood value \code{llh}, 
  a boolean vector stating which parameters were fitted
  \code{fitted.params}, the Akaike Information Criterion \code{aic},
  a boolean \code{converged} whether the fitting procedure converged or not, 
  an \code{error.code} which stores the status of a possible error, 
  the corresponding \code{error.message} and the \code{parameter.variance}.}
  
\section{Objects from the Class}{
  Objects should only be created by calls to the fitting routines like \code{fit.ghypuv},
  \code{fit.hypuv}, \code{fit.NIGuv}, \code{fit.VGuv} and \code{fit.tuv}.
}
\section{Slots}{
	 \describe{
    \item{\code{lambda}:}{Shape parameter of class \code{"numeric"}.}
    \item{\code{alpha.bar}:}{Shape parameter of class \code{"numeric"}.}
    \item{\code{chi}:}{Shape parameter of an alternative parametrization.
                      Object of class \code{"numeric"}.}
    \item{\code{psi}:}{Shape parameter of an alternative parametrization.
                      Object of class \code{"numeric"}.}
    \item{\code{mu}:}{Location parameter of lass \code{"numeric"}.}
    \item{\code{sigma}:}{Dispersion parameter of class \code{"numeric"}.}
    \item{\code{gamma}:}{Skewness parameter of class \code{"numeric"}.}
    \item{\code{model}:}{Model, i.e., (a)symmetric generalized hyperbolic distribution or 
                         (a)symmetric special case. Object of class \code{"character"}.}
    \item{\code{dimension}:}{Dimension of the generalized hyperbolic distribution. 
                             Object of class \code{"numeric"}.}
    \item{\code{expected.value}:}{The expected value of a generalized 
                                  hyperbolic distribution.
                                  Object of class \code{"numeric"}.}
    \item{\code{variance}:}{The expected value of a generalized 
                                  hyperbolic distribution.
                                  Object of class \code{"numeric"}.}
    \item{\code{data}:}{Data vector of class \code{"numeric"}. When an object of class 
                        \code{ghypuv} is instantiated the user can decide whether
                        data should be stored within the object or not. This may be useful
                        when fitting eneralized hyperbolic distributions to data and 
                        perform further analysis afterwards.}
    \item{\code{n.iter}:}{The number of iterations of class \code{"numeric"}.}
    \item{\code{llh}:}{The log likelihood value of class \code{"numeric"}.}
    \item{\code{converged}:}{A boolean whether \code{\link{optim}} converged or not.
                             Object of class \code{"logical"}.}
    \item{\code{error.code}:}{An error code of class \code{"numeric"}.}
    \item{\code{error.message}:}{An error message of class \code{"character"}.}
    \item{\code{parameter.variance}:}{The parameter variance is calculated to be the inverse of the fisher information matrix. 
         Parameter.variance is of class \code{"matrix"}.}
    \item{\code{fitted.params}:}{A boolean vector stating which parameters were fitted of class \code{"logical"}.}
    \item{\code{aic}:}{The value of the Akaike Information Criterion of class \code{"numeric"}.}  
  }
}
\section{Extends}{
Class \code{"\linkS4class{ghypuv}"}, directly.
Class \code{"\linkS4class{ghypbase}"}, by class "ghypuv", distance 2.
}
\section{Methods}{
  A \dQuote{hist} method (see \code{\link[=hist.ghypuv]{hist}}).\cr
   A \dQuote{mean} method (see \code{\link[=mean.ghypbase]{mean}}). \cr
   A \dQuote{vcov} method (see \code{\link[=vcov.ghypbase]{vcov}}).   
}
\author{David Lüthi}
\seealso{
  \code{\link{optim}} for an interpretation of \code{error.code} and \code{error.message}, 
  \code{\link{fit.ghypuv}} where objects of class \code{mle.ghypuv} are created,
  \code{\link{ghypuv-class}} to have a look on the base class.

}
\note{
  When showing special cases of the generalized hyperbolic distribution
  the corresponding fixed parameters are plotted in brackets.
}
\examples{
  data(smi.stocks)
  fit.ghypuv(data=smi.stocks[,"SMI"],opt.pars=c(alpha.bar=FALSE,lambda=FALSE),
             alpha.bar=1)
}
\keyword{classes}
