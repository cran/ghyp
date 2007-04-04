\name{mle.ghypmv-class}
\docType{class}
\alias{mle.ghypmv-class}
\alias{show.mle.ghypmv}
\alias{show,mle.ghypmv-method}
\title{Class mle.ghypmv}
\description{The class \dQuote{mle.ghypmv} inherits from the class \dQuote{ghypmv}. In addition
  to the class \dQuote{ghypmv} this class stores fitting information. Namely the
  number of iterations \code{n.iter}, the log likelihood value \code{llh}, 
  the Akaike Information Criterion \code{aic}, a boolean vector stating which parameters were fitted
  \code{fitted.params},
  a boolean \code{converged} whether the fitting procedure converged or not, 
  an \code{error.code} which stores the status of a possible error and 
  the corresponding \code{error.message}}
\section{Objects from the Class}{
  Objects should only be created by calls to the fitting routines like \code{fit.ghypmv},
  \code{fit.hypmv}, \code{fit.NIGmv} , \code{fit.VGmv} and \code{fit.tmv} .
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
    \item{\code{sigma}:}{Dispersion parameter of class \code{"matrix"}.}
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
                                  Object of class \code{"matrix"}.}
    \item{\code{data}:}{Data matrix of class \code{"matrix"}. When an object of class 
                        \code{ghypmv} is instantiated the user can decide whether
                        data should be stored within the object or not. This may be useful
                        when fitting eneralized hyperbolic distributions to data and 
                        perform further analysis afterwards.}
    \item{\code{n.iter}:}{The number of iterations of class \code{"numeric"}.}
    \item{\code{llh}:}{The log likelihood value of class \code{"numeric"}.}
    \item{\code{converged}:}{A boolean whether converged or not.
                             Object of class \code{"logical"}.}
    \item{\code{error.code}:}{An error code of class \code{"numeric"}.}
    \item{\code{error.message}:}{An error message of class \code{"character"}.}
    \item{\code{fitted.params}:}{A boolean vector stating which parameters were fitted of class \code{"logical"}.}
    \item{\code{aic}:}{The value of the Akaike Information Criterion of class \code{"numeric"}.}    
  }
}
\section{Extends}{
Class \code{"\linkS4class{ghypmv}"}, directly.
Class \code{"\linkS4class{ghypbase}"}, by class "ghypmv", distance 2.
}
\section{Methods}{
   A \dQuote{pairs} method (see \code{\link[=pairs.ghypmv]{pairs}}).\cr
   A \dQuote{mean} method (see \code{\link[=mean.ghypbase]{mean}}). \cr
   A \dQuote{vcov} method (see \code{\link[=vcov.ghypbase]{vcov}}). 
}
\author{David Lüthi}
\note{
  When showing special cases of the generalized hyperbolic distribution
  the corresponding fixed parameters are plotted in brackets.
}
\seealso{
  \code{\link{optim}} for an interpretation of \code{error.code} and \code{error.message}, 
  \code{\link{fit.ghypmv}} where objects of class \code{mle.ghypuv} were created,
  \code{\link{ghypmv-class}} to have a look on the base class.
}
\examples{
  data(smi.stocks)
  fit.ghypmv(data=smi.stocks,opt.pars=c(lambda=FALSE, alpha.bar=FALSE),lambda=2)
}
\keyword{classes}

