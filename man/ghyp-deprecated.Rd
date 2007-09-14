\name{ghyp-deprecated}
\alias{ghyp.params}
\alias{lin.transf}
\alias{redim}
\title{Deprecated Functions in ghyp package}
\description{
  These functions are provided for compatibility with older versions of 
  the package \code{ghyp} only, and will be defunct in the next release.
}
\usage{
ghyp.params(object, type = c("chipsi", "alpha.bar"))

redim(x, i = c(1, 2))

lin.transf(x, \dots)
}
\arguments{
  \item{object}{An object inheriting from class \code{\link[=ghyp-class]{ghyp}}.}
  \item{type}{According to \code{type} the parameters of either the \dQuote{chi/psi} or the \dQuote{alpha.bar}
              parametrization will be returned.} 
  \item{x}{A (multivariate) generalized hyperbolic distribution inheriting from class \code{\link[=ghyp-class]{ghyp}}.}
  \item{i}{Index specifying which dimensions to extract.}
  \item{\dots}{Arguments passed to \code{transform}.}                
}
\value{
  For \code{ghyp.params} see \code{\link[=coef.ghyp]{coef}}, for \code{redim} see \code{\link[=[.ghyp]{[}},
  for \code{lin.transf} see \code{\link[=transform.ghyp]{transform}}.
}
\author{David Lüthi}
\seealso{\code{\link[=coef.ghyp]{coef}}, \code{\link[=[.ghyp]{[}}, 
         \code{\link[=transform.ghyp]{transform}}
}
\keyword{misc}