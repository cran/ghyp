\name{portfolio.optimize}
\alias{portfolio.optimize}
\title{Portfolio optimization given a multivariate generalized hyperbolic distribution}
\description{
  This function performs a optimization of a portfolio with respect to one
  of the risk measures \dQuote{variance}, \dQuote{quantile} or \dQuote{expected-shortfall}, 
  a level of risk and the requested portfolio return
  given a multivariate generalized hyperbolic distribution.
}
\usage{
portfolio.optimize(object, ptf.mean = 0.01, 
           risk.measure = c("variance", "quantile", "expected-shortfall"), 
           level = 0.95,...)
}
\arguments{
  \item{object}{A multivariate generalized hyperbolic object.}
  \item{ptf.mean}{The required expected return of the portfolio.}
  \item{risk.measure}{The risk measure to which the portfolio should be optimized.
                      Must be one of \dQuote{variance}, \dQuote{quantile} or
                      \dQuote{expected-shortfall}.}
  \item{level}{The level of the \code{risk.measure}. Only used when \code{risk.measure}
               is \dQuote{quantile} or \dQuote{expected-shortfall}.}
  \item{\dots}{Arguments passed to \code{\link{optim}}.}
}

\value{
  A list with components:
  \item{portfolio}{An object of class \code{\link[=ghypuv-class]{ghypuv}} which 
                   represents the generalized hyperbolic distribution of the portfolio.}
  \item{risk.measure}{The optimization criterion.}
  \item{value}{The value of the risk measure.}
  \item{opt.weights}{The optimal weights.}
  \item{convergence}{Convergence returned from \code{\link{optim}}.}    
  \item{message}{A possible error message returned from \code{\link{optim}}.}    
  \item{n.iter}{The number of iterations returned from \code{\link{optim}}.}      
}
\author{David Lüthi}
\note{
  There is no constraint implemented yet determining whether 
  short selling is allowed or not.  \cr
  
  If the risk measure is \dQuote{variance} the returned portfolio is simply an
  efficient frontier.
}
\seealso{\code{\link{lin.transf}}, \code{\link{fit.ghypmv}}}
\examples{

}
\keyword{optimize}
\keyword{multivariate}
\keyword{iteration}
