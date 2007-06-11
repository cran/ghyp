\name{ghypbase-class}
\docType{class}
\alias{ghypbase-class}
\title{Class ghypbase}
\description{
  Base class of \code{\link[=ghypuv-class]{ghypuv}} and \code{\link[=ghypmv-class]{ghypmv}}.
}
\section{Objects from the Class}{
  No objects should be created from this class.
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
    \item{\code{gamma}:}{Skewness parameter of class \code{"numeric"}.}
    \item{\code{model}:}{Model, i.e., (a)symmetric generalized hyperbolic distribution or 
                         (a)symmetric special case. Object of class \code{"character"}.}
    \item{\code{dimension}:}{Dimension of the generalized hyperbolic distribution. 
                             Object of class \code{"numeric"}.}
    \item{\code{expected.value}:}{The expected value of a generalized 
                                  hyperbolic distribution.
                                  Object of class \code{"numeric"}.}
  }
}
\section{Methods}{
   A \dQuote{mean} method (see \code{\link[=mean.ghypbase]{mean}}). \cr
   A \dQuote{vcov} method (see \code{\link[=vcov.ghypbase]{vcov}}).   
}
\author{ David Luethi }
\seealso{
	See \code{\link[=ghypuv-class]{ghypuv}} or \code{\link[=ghypmv-class]{ghypmv}} for classes
	which can be instantiated.
}
\keyword{classes}


