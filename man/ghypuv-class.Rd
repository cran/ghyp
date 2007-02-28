\name{ghypuv-class}
\docType{class}
\alias{ghypuv-class}
\alias{show.ghypuv}
\alias{show,ghypuv-method}
\title{Class ghypuv}
\description{The class \dQuote{ghypuv} inherits from the base class \dQuote{ghypbase}. All the
  parameters, the alternative parameters, the expected value, the variance and the
  model are stored in an object of class \dQuote{ghypuv}.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form 
  \code{ghyp(lambda=1, alpha.bar=0.1, mu=0, sigma=1, gamma=0)}.
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
    \item{\code{model}:}{Model, i.e., generalized hyperbolic distribution or 
                         (a)symmetric special case. Object of class \code{"character"}.}
    \item{\code{dimension}:}{Dimension of the generalized hyperbolic distribution. 
                             Object of class \code{"numeric"}.}
    \item{\code{expected.value}:}{The expected value of a generalized 
                                  hyperbolic distribution.
                                  Object of class \code{"numeric"}.}
    \item{\code{variance}:}{The expected value of a generalized 
                                  hyperbolic distribution.
                                  Object of class \code{"numeric"}.}
    \item{\code{data}:}{Data of class \code{"numeric"}. When an object of class 
                        \code{ghypuv} is instantiated the user can decide whether
                        data should be stored within the object or not. This may be useful
                        when fitting generalized hyperbolic distributions to data and 
                        perform further analysis afterwards.}
  
  }
}
\section{Extends}{
  Class \code{"\linkS4class{ghypbase}"}, directly.
}
\section{Methods}{
   A \dQuote{hist} method (see \code{\link[=hist.ghypuv]{hist}}). \cr
   A \dQuote{mean} method (see \code{\link[=mean.ghypbase]{mean}}). \cr
   A \dQuote{vcov} method (see \code{\link[=vcov.ghypbase]{vcov}}).   
}
\author{ David Lüthi }
\seealso{
  \code{\link{ghyp}}, \code{\link{mle.ghypuv-class}} and  \code{\link{fit.ghypuv}}
}
\examples{
  ghyp(lambda=1, alpha.bar=0.1, mu=0, sigma=1, gamma=0)
}
\keyword{classes}
