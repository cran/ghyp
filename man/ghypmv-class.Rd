\name{ghypmv-class}
\docType{class}
\alias{ghypmv-class}
\alias{show.ghypmv}
\alias{show,ghypmv-method}
\title{Class ghypmv}
\description{The class \dQuote{ghypmv} inherits from the base class \dQuote{ghypbase}. All the
parameters, the alternative parameters, the expected value, the variance and the
model are stored in an object of class \dQuote{ghypmv}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form 
\code{ghyp(lambda=1, alpha.bar=0.1, mu=rep(0,2), sigma=diag(rep(1,2)), gamma=rep(0,2))}.
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
    \item{\code{data}:}{Data of class \code{"matrix"}. When an object of class 
                        \code{ghypmv} is instantiated the user can decide whether the
                        data should be stored within the object or not. This may be useful
                        when fitting eneralized hyperbolic distributions to data and 
                        perform further analysis afterwards.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{ghypbase}"}, directly.
}
\section{Methods}{
   A \dQuote{pairs} method (see \code{\link[=pairs.ghypmv]{pairs}}). \cr
   A \dQuote{mean} method (see \code{\link[=mean.ghypbase]{mean}}). \cr
   A \dQuote{vcov} method (see \code{\link[=vcov.ghypbase]{vcov}}). 
}
\author{David Lüthi}
\seealso{
\code{\link{ghyp}}, \code{\link{mle.ghypmv-class}} and  \code{\link{fit.ghypmv}}
}
\examples{
  ghyp(lambda=1, alpha.bar=0.1, mu=rep(0,2), sigma=diag(rep(1,2)), gamma=rep(0,2))
}
\keyword{classes}
