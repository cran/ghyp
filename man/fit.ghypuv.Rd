\name{fit.ghypuv}
\alias{fit.ghypuv}
\alias{fit.hypuv}
\alias{fit.NIGuv}
\alias{fit.VGuv}
\alias{fit.tuv}
\title{Fitting generalized hyperbolic distributions to univariate data}
\description{
   This function performs a maximum likelihood parameter estimation for univariate 
   generalized hyperbolic distributions.
}
\usage{
fit.ghypuv(data, lambda = 1, alpha.bar = 0.1, mu = mean(data), 
           sigma = sd(data), gamma = 0, 
           opt.pars = c(lambda = T, alpha.bar = T, mu = T, 
                        sigma = T, gamma = !symmetric), 
           symmetric = F, standardize = F, save.data = T, 
           na.rm = T, silent = FALSE, ...)
           
fit.hypuv(data, 
          opt.pars = c(alpha.bar = T, mu = T, sigma = T, gamma = !symmetric), 
          symmetric = F, ...)

fit.NIGuv(data, 
          opt.pars = c(alpha.bar = T, mu = T, sigma = T, gamma = !symmetric), 
          symmetric = F, ...)

fit.VGuv(data, lambda = 1, 
         opt.pars = c(lambda = T, mu = T, sigma = T, gamma = !symmetric), 
         symmetric = F, ...)

fit.tuv(data, nu = 3.5, 
        opt.pars = c(nu = T, mu = T, sigma = T, gamma = !symmetric), 
        symmetric = F, ...)
}
\arguments{
  \item{data}{An object coercible to a \code{vector}.}
  \item{lambda}{Shape parameter.}
  \item{alpha.bar}{Shape parameter.}
  \item{nu}{Shape parameter only used in case of a student-t distribution. It determines
            the degree of freedom and is defined as \code{-2*lambda}.}
  \item{mu}{Location parameter.}
  \item{sigma}{Dispersion parameter.}
  \item{gamma}{Skewness parameter.}
  \item{opt.pars}{A named logical \code{vector} which states which parameters should be fitted.}
  \item{symmetric}{If \code{TRUE} the skewness parameter \code{gamma} keeps zero.}
  \item{standardize}{If \code{TRUE} the sample will be standardized before fitting. 
                     Afterwards, the parameters and log-likelihood et cetera will be back-transformed.}
  \item{save.data}{If \code{TRUE} \code{data} will be stored within the 
                   \code{\link[=mle.ghyp-class]{mle.ghyp}} object.}
  \item{na.rm}{If \code{TRUE} missing values will be removed from \code{data}.}
  \item{silent}{If \code{TRUE} no prompts will appear in the console.}
  \item{\dots}{Arguments passed to \code{\link{optim}} and to \code{fit.ghypuv} when
               fitting special cases of the generalized hyperbolic distribution.}
}
\details{
 The general-purpose optimization routine \code{\link{optim}} is used to maximize
 the loglikelihood function. The default method is that of Nelder and Mead which
 uses only function values. Parameters of \code{\link{optim}} can be passed via
 the \dots  argument of the fitting routines.
}
\value{
  An object of class \code{\link[=mle.ghyp-class]{mle.ghyp}}.
}
\author{
  Wolfgang Breymann, David Lüthi
}
\note{
  The variance gamma distribution becomes singular when \code{x - m = 0}. This singularity
  is catched and the reduced density function is computed. Because the transition is
  not smooth in the numerical implementation this can rarely result in nonsensical
  fits.
  
  Providing both arguments, \code{opt.pars} and \code{symmetric} respectively,
  can result in a conflict when \code{opt.pars['gamma']} and \code{symmetric}
  are \code{TRUE}. In this case \code{symmetric} will dominate and 
  \code{opt.pars['gamma']} is set to \code{FALSE}.
}
\seealso{\code{\link{fit.ghypmv}}, \code{\link{fit.hypmv}}, \code{\link{fit.NIGmv}},
         \code{\link{fit.VGmv}}, \code{\link{fit.tmv}} for multivariate fitting routines.
}
\examples{
  data(smi.stocks)
  fit.NIGuv(data = smi.stocks[,"SMI"], opt.pars = c(alpha.bar = FALSE),
            alpha.bar = 1, control = list(abs.tol = 1e-5, maxit = 100))
}
\keyword{iteration}
\keyword{optimize}
\keyword{distribution}
\keyword{models}