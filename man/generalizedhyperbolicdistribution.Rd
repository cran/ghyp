\name{Generalized Hyperbolic Distribution}
\alias{dghyp}
\alias{pghyp}
\alias{rghyp}
\alias{qghyp}
\alias{ESghyp}
\title{The Generalized Hyperbolic Distribution}
\description{
  Density, distribution function, quantile function, expected shortfall and
  random generation for the univariate and multivariate
  generalized hyperbolic distribution and its special cases.
}
\usage{
dghyp(x, object = ghyp(), logvalue = F)

pghyp(q, object = ghyp(), n.sim = 10000, subdivisions = 200, 
      rel.tol = .Machine$double.eps^0.5, abs.tol = rel.tol)

qghyp(p, object = ghyp(), method = c("integration", "splines"), 
      spline.points = 200, subdivisions = 200, 
      root.tol = .Machine$double.eps^0.5, 
      rel.tol = root.tol^1.5, abs.tol = rel.tol)

rghyp(n, object = ghyp())

ESghyp(p, object = ghyp(), ...)
}
\arguments{
  \item{p}{A vector of probabilities.}
  \item{x}{A vector, matrix or data.frame of quantiles.}
  \item{q}{A vector, matrix or data.frame of quantiles.}
  \item{n}{Number of observations.}
  \item{object}{An object inheriting from class \code{\link[=ghypbase-class]{ghypbase}}.}
  \item{logvalue}{If \code{TRUE} the logarithm of the density will be returned.}
  \item{n.sim}{The number of simulations when computing \code{pghyp} of a multivariate
               generalized hyperbolic distribution.}
  \item{subdivisions}{The number of subdivisions passed to \code{\link{integrate}} when
                      computing the distribution function \code{pghyp} of a 
                      univariate generalized hyperbolic distribution.}
  \item{rel.tol}{The relative accuracy requested from  \code{\link{integrate}}.}
  \item{abs.tol}{The absolute accuracy requested from  \code{\link{integrate}}.}
  \item{method}{The method how quantiles are computed.}
  \item{spline.points}{The number of support points when computing the quantiles using
                       \code{\link[=spline]{splines}} instead of integration.}
  \item{root.tol}{The tolerance of \code{\link{uniroot}}.}
  \item{\dots}{Arguments passed from \code{ESghyp} to \code{qghyp}.}  
}
\details{
   \code{qghyp} and \code{ESghyp} do only work for univariate generalized hyperbolic
   distributions. \cr
   
   \code{pghyp} is a crude implementation of the distribution function for 
   multivariate generalized hyperbolic distributions by means of monte carlo calculation. \cr
   
   \code{qghyp} computes the quantiles either by using the \dQuote{integration} method where the root of 
   the distribution function is solved or via \dQuote{splines} which interpolates the distribution
   function and solves it with \code{\link{uniroot}} afterwards. The \dQuote{integration}
   method is recommended when few quantiles are required. If more than approximately
   20 quantiles are needed to be calculated the \dQuote{splines} method becomes faster.
   The accuracy can be controlled with an adequate setting of the
   parameters \code{rel.tol}, \code{abs.tol}, \code{root.tol} and \code{spline.points}.\cr
   
   \code{rghyp} uses the random generator for generalized inverse gaussian distributed
    random variates from the S-Plus library \emph{QRMlib} 
  (see \url{http://www.math.ethz.ch/~mcneil/book/QRMlib.html}).
}
\value{
  \code{dghyp} gives the density, \cr \code{pghyp} gives the distribution function, \cr
  \code{qghyp} gives the quantile function, \cr \code{ESghyp} gives the expected shortfall and  \cr
  \code{rghyp} generates random deviates.
}
\references{
  Alexander J. McNeil, Rüdiger Frey, Paul Embrechts (2005) \emph{Quantitative
    Risk Management, Concepts, Techniques and Tools}  \cr
}
\author{David Lüthi}
\note{
  Objects generated with \code{hyp}, \code{NIG}, \code{VG} and \code{student.t} have
  to use \code{Xghyp} as well. E.g. \code{dNIG(NIG())} does not work.  \cr
  
  When the skewness becomes very large the functions using \code{qghyp} may fail. 
  The functions using \code{qghyp} are \code{ESghyp}, \code{qqghyp}, \code{pairs} 
  and \code{portfolio.optimize}. 
}
\seealso{\code{\link{ghypuv-class}}, \code{\link{ghypmv-class}},
         \code{\link{fit.ghypuv}}, \code{\link{fit.ghypmv}}, \code{\link{integrate}},
         \code{\link{uniroot}}.}
\examples{
  ## Univariate generalized hyperbolic distribution
  univariate.ghyp <- ghyp()
  par(mfrow=c(5,1))
  quantiles <- seq(-4,4,length=500)
  plot(quantiles,dghyp(quantiles,univariate.ghyp))
  plot(quantiles,pghyp(quantiles,univariate.ghyp))
  probabilities <- seq(1e-4,1-1e-4,length=500)
  plot(probabilities,qghyp(probabilities,univariate.ghyp,method="splines"))
  plot(probabilities,ESghyp(probabilities,univariate.ghyp,method="splines"))
  hist(rghyp(n=10000,univariate.ghyp),nclass=100)
  ## Mutivariate generalized hyperbolic distribution
  multivariate.ghyp <- ghyp(sigma=var(matrix(rnorm(10),ncol=2)),mu=1:2,gamma=-(2:1))
  par(mfrow=c(2,1))
  quantiles <- outer(seq(-4,4,length=50),c(1,1))
  plot(quantiles[,1],dghyp(quantiles,multivariate.ghyp))
  plot(quantiles[,1],pghyp(quantiles,multivariate.ghyp,n.sim=1000))
  rghyp(n=10,multivariate.ghyp)
}
\keyword{distribution}
\keyword{models}
\keyword{multivariate}
\keyword{datagen}
