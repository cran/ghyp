\name{ghyp-internal}
\alias{ghyp.internal}
\title{Internal ghyp functions}
\description{Internal ghyp functions. These functions are not to be called by the user.
}
\usage{
abar2chipsi(alpha.bar, lambda, eps = .Machine$double.eps) 

besselM3(lambda = 9/2, x = 2, logvalue = FALSE)

check.data(data, case = c("uv", "mv"), na.rm = T, fit = TRUE, dim = NULL)

check.gig.pars(lambda, chi, psi)
                        
check.norm.pars(mu, sigma, gamma)

check.opt.pars(opt.pars)

fit.ghyp(object, llh = 0, n.iter = 0, converged = FALSE, error.code = 0, 
         error.message = "", parameter.variance, fitted.params, aic)
         
gh.model(lambda, chi, psi, gamma) 

identity(x)

t.transform(lambda)    

integrate.moment.gig(x, moment = 1, ...)

integrate.moment.ghypuv(x, moment = 1, ...)  

internal.dghyp(x, lambda = 1, chi = 1, psi = 1, alpha.bar = NULL, 
               mu = 1, sigma = 1, gamma = 0, logvalue = F)

internal.dghypmv(x, lambda, chi, psi, mu, sigma, gamma, logvalue = F)  

mle.default(data, pdf, vars, opt.pars = rep(F, length(vars)), 
            transform = rep("identity", length(vars)), se = F, 
            na.rm = F, silent = FALSE, ...)
            
p.default(q,pdf,pdf.args,lower,...)

q.default(p, pdf, pdf.args, interval, p.lower, ...)

test.class.ghyp(object, case = c("ghypbase","ghypuv","ghypmv"))

llh.surface(obj,param=c("alpha.bar","lambda","mu","sigma","gamma"),
            x.seq=NULL,y.seq=NULL,x.range=c(0,1),y.range=c(0,1),
            n.grid=100, percent=FALSE,plot.it=TRUE)
}
\details{
\code{abar2chipsi} \cr
  Convert \dQuote{alpha.bar} to \dQuote{chi} and \dQuote{psi} when using the 
  \dQuote{alpha.bar} parametrization.

\code{besselM3} \cr
  Wrapper function for \code{\link{besselK}}.

\code{check.data} \cr
  This function checks \code{data} for consistency.
  Only data objects of typ \code{data.frame},
  \code{matrix} or \code{numeric} are accepted.

\code{check.gig.pars} \cr
  Some combinations of the GIG parameters are not allowed. This 
  function checks whether this is the case or not. 

\code{check.norm.pars} \cr
This function simply checks if the dimensions match.
  
\code{check.gig.pars} \cr
  Some combinations of the GIG parameters are not allowed. This 
  function checks whether the parameters are valid or not. 

\code{check.opt.pars} \cr
  When calling the fitting routines (\code{\link{fit.ghypuv}} and \code{\link{fit.ghypmv}})
  a named vector containing the parameters which should not be fitted can be passed. 
  By default all parameters will be fitted.

\code{fit.ghyp} \cr
  This function is called by the functions
  \code{\link{fit.ghypuv}} and \code{\link{fit.ghypmv}} to create
  objects of class \code{\link[=mle.ghypuv-class]{mle.ghypuv}} and 
  \code{\link[=mle.ghypmv-class]{mle.ghypmv}}.

\code{gh.model} \cr
  Check if the parameters denote a special case of the generalized hyperbolic
  distribution.

\code{identity} \cr
  Identity function used in \code{\link{fit.ghypuv}} as a
  transformation function of certain parameters which have to be optimized numerically. 

\code{t.transfrom} \cr
  Transformation  function used in \code{\link{fit.ghypuv}} for
  certain parameters which have to be optimized numerically. 

\code{integrate.moment.gig} \cr
  This function is used when computing the conditional expectation
  of a generalized inverse gaussian distribution.

\code{integrate.moment.ghypuv} \cr
  This function is used when computing the conditional expectation
  of a univariate generalized hyperbolic distribution.

\code{internal.dghyp} \cr
  This function is used during the fitting procedure. Use \code{\link{dghyp}} 
  to compute the density of generalized hyperbolic distribution
  objects.

\code{internal.dghypmv} \cr
  This function is used during the fitting procedure. Use \code{\link{dghyp}} 
  to compute the density of generalized hyperbolic distribution
  objects. 
  
\code{mle.default} \cr
  This function serves as a generic function for maximum likelihood estimation.
  It is for internal use only. See \code{\link{fit.ghypuv}} which wraps this
  function. 

\code{p.default} \cr
  A generic distribution function integrator given a density function.
  See \code{\link{pghyp}} for a wrapper of this
  function.

\code{q.default} \cr
  A generic quantile function calculator given a density function. 
  See \code{\link{qghyp}} for a wrapper of this   function.
  
\code{test.class.ghyp} \cr
  This function tests whether the object is of class
  \code{\link[=ghypbase-class]{ghypbase}}, \code{\link[=ghypuv-class]{ghypuv}} or
  \code{\link[=ghypmv-class]{ghypmv}} according to the argument \code{case}
  and states a corresponding error if not.
  
\code{llh.surface} \cr
  This function is intended to draw surface plots of the log-likelihood function 
  either over a one or two-dimensional parameter space.
}
\author{Wolfgang Breymann, David Lüthi}
\keyword{internal}
