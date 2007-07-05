\name{ghyp-package}
\alias{ghyp-package}
\docType{package}
\title{
A package on generalized hyperbolic distributions
}
\description{
Description: This package provides all about univariate and multivariate generalized 
hyperbolic distributions and its special cases (Hyperbolic (hyp), Normal Inverse Gaussian (NIG), 
Variance Gamma (VG) and skewed Student-t distribution). Especially fitting procedures, 
an AIC-based model selection routine and functions for the
computation of the density, quantile, probability, random variates, expected shortfall 
and some portfolio optimization and plotting routines. 
In addition the generalized inverse gaussian distribution is contained in this package.
}
\details{
\tabular{ll}{
Package: \tab ghyp\cr
Type: \tab Package\cr
Version: \tab 0.9.3; svn-revision 132\cr
Date: \tab 2007-07-05\cr
License: \tab  GPL (GNU Public Licence), Version 2 or later \cr
}

\strong{Initialize:}
\tabular{ll}{
  \code{\link{ghyp}} \tab             Initialize a generalized hyperbolic distribution\cr
  \code{\link{hyp}} \tab              Initialize a hyperbolic distribution\cr
  \code{\link{NIG}} \tab              Initialize a normal inverse gaussian distribution\cr
  \code{\link{VG}} \tab               Initialize a variance gamma distribution\cr
  \code{\link{student.t}} \tab        Initialize a student-t distribution\cr
}

\strong{Density, distribution function, quantile function, expected shortfall and
  random generation:}
\tabular{ll}{
  \code{\link{dghyp}} \tab         Density of a generalized hyperbolic distribution\cr
  \code{\link{pghyp}} \tab         Distribution function of a generalized hyperbolic distribution\cr
  \code{\link{qghyp}} \tab         Quantile of a univariate generalized hyperbolic distribution\cr
  \code{\link{ESghyp}} \tab        Expected shortfall of a univariate generalized hyperbolic distribution\cr
  \code{\link{rghyp}} \tab         Random generation of a generalized hyperbolic distribution\cr
}

\strong{Fit to data:}
\tabular{ll}{
  \code{\link{fit.ghypuv}} \tab     Fit a generalized hyperbolic distribution to univariate data\cr
  \code{\link{fit.hypuv}} \tab      Fit a hyperbolic distribution to univariate data\cr
  \code{\link{fit.NIGuv}} \tab      Fit a normal inverse gaussian distribution to univariate data\cr
  \code{\link{fit.VGuv}} \tab       Fit a variance gamma distribution to univariate data\cr
  \code{\link{fit.tuv}} \tab        Fit a skewed student-t distribution to univariate data\cr      
  \code{\link{fit.ghypmv}} \tab     Fit a generalized hyperbolic distribution to multivariate data\cr
  \code{\link{fit.hypmv}} \tab      Fit a hyperbolic distribution to multivariate data\cr
  \code{\link{fit.NIGmv}} \tab      Fit a normal inverse gaussian distribution to multivariate data\cr
  \code{\link{fit.VGmv}} \tab       Fit a variance gamma distribution to multivariate data\cr
  \code{\link{fit.tmv}} \tab        Fit a skewed student-t distribution to multivariate data\cr      
  \code{\link{stepAIC.ghyp}} \tab   Perform a model selection based on the AIC\cr     
}

\strong{Portfolio optimization and utilities:}
\tabular{ll}{
  \code{\link{portfolio.optimize}} \tab     Calculate an optimal portfolio given a multivariate \code{ghyp} distribution\cr
  \code{\link[=mean.ghypbase]{mean}} \tab   Returns the expected value\cr
  \code{\link[=vcov.ghypbase]{vcov}} \tab   Returns the variance in the univariate case or else the variance covariance matrix\cr
  \code{\link[=logLik.mle.ghyp]{logLik}} \tab   Returns Log-Likelihood of fitted ghyp objects.\cr
  \code{\link[=AIC.mle.ghyp]{AIC}} \tab     Returns Akaike's Information Criterion  of fitted ghyp objects.\cr  
  \code{\link{redim}} \tab                  Extract certain dimensions of a multivariate \code{ghyp} distribution\cr
  \code{\link{lin.transf}} \tab             Transform a multivariate generalized hyperbolic distribution\cr
  \code{\link{ghyp.moment}} \tab            Moments of the univariate \code{ghyp} distribution\cr
  \code{\link{ghyp.params}} \tab            Parameters of a generalized hyperbolic distribution \cr
  \code{\link{ghyp.data}} \tab              Data of a (fitted) generalized hyperbolic distribution\cr
  \code{\link{ghyp.fit.info}} \tab          Information about the fitting procedure, log-likelihood and AIC value\cr      
}

\strong{Plot functions:}
\tabular{ll}{
  \code{\link{qqghyp}} \tab              Perform a quantile-quantile plot of a (fitted) univariate \code{ghyp} distribution\cr
  \code{\link[=hist.ghypuv]{hist}} \tab         Plot a histogram of a (fitted) univariate generalized hyperbolic distribution\cr
  \code{\link[=pairs.ghypmv]{pairs}} \tab        Produce a matrix of scatterplots with quantile-quantile plots on the diagonal.\cr
}

\strong{Generalized inverse gaussian distribution:} 
\tabular{ll}{
  \code{\link{dgig}} \tab          Density of a generalized inverse gaussian distribution\cr
  \code{\link{pgig}} \tab          Distribution function of a generalized inverse gaussian distribution\cr
  \code{\link{qgig}} \tab          Quantile of a generalized inverse gaussian distribution\cr
  \code{\link{ESgig}} \tab         Expected shortfall of a generalized inverse gaussian distribution\cr
  \code{\link{rgig}} \tab          Random generation of a generalized inverse gaussian distribution\cr
}

\strong{Package vignette:} \cr
A document about generalized hyperbolic distributions can be found in the
\code{doc} folder of this package. 
}
%---------------------------------------------------------------------------------------
\section{Existing solutions}{
  There are already two packages \code{HyperbolicDist} and \code{fBasics} which cover 
  univariate generalized hyperbolic distributions. However, the univariate case is contained
  in this package as well because we aim to provide a uniform interface to deal with
  generalized hyperbolic distribution. Recently an \R port of the S-Plus library \code{QRMlib}
  was released. The package \code{QRMlib} contains fitting procedure for the NIG, hyp and
  skewed Student-t case but not for the generalized hyperbolic case.
  The package \code{fMultivar} implements
  a fitting routine for multivariate skewed student-t distributions as well. 
  The table below gives an overview of available packages concerning generalized 
  hyperbolic distributions and their capabilities.

  \tabular{lccccc}{
  %<------------------------------------------------------------>
  \tab \bold{ghyp} \tab \bold{Hyperbolic} \tab \bold{fBasics} \tab \bold{QRMlib}  \tab \bold{multi\_GHD} \cr
  \tab \tab \bold{Dist} \tab \tab \bold{(R \& S-Plus)} \tab \bold{(Matlab)} \cr

  %<------------------------------------------------------------>
  \bold{Univariate:} \tab \tab \tab \tab \tab \cr
  Density             \tab     \code{x}    \tab \code{x} \tab \code{x} \tab \tab \cr
  Distribution        \tab   \code{x}    \tab \code{x} \tab \code{x} \tab \tab \cr
  Quantile            \tab \code{x}    \tab \code{x} \tab \code{x} \tab \tab \cr
  Random Generation   \tab \code{x}    \tab \code{x} \tab \code{x} \tab \tab \cr
  GH special cases    \tab hyp, NIG, \tab hyp, VG \tab hyp, NIG \tab \tab \cr
                      \tab VG, skew-t \tab \tab \tab \tab \cr  
  Fit                 \tab  \code{x}    \tab only hyp \tab \code{x} \tab \tab \cr
  Constant parameters \tab  \code{x}    \tab \tab \tab \tab \cr

  %<------------------------------------------------------------>
  \bold{Multivariate:} \tab \tab \tab \tab \tab \cr
  Density             \tab     \code{x} \tab \tab  \tab \code{x} \tab \code{x}  \cr
  Distribution        \tab   \code{x}    \tab \tab \tab \tab \cr
  Random Generation   \tab \code{x}  \tab \tab \tab \code{x} \tab \code{x}\cr
  GH special cases    \tab hyp, NIG,  \tab \tab \tab hyp, NIG \tab \cr
                      \tab VG, skew-t \tab \tab \tab \tab \cr
  Fit                 \tab  \code{x}    \tab \tab \tab only hyp, NIG, skew-t \tab \code{x}\cr
  Constant parameters \tab  \code{x}    \tab \tab \tab \tab \cr

  %<------------------------------------------------------------>
  \bold{Diagnostic plots:} \tab \tab \tab \tab \tab \cr
  hist              \tab  \code{x} \tab \code{x} \tab \code{x} \tab \tab \cr
  Quantile-quantile \tab  \code{x} \tab \code{x} \tab \tab \tab \cr
  pairs             \tab  \code{x} \tab \tab \tab \tab \cr
  }
}
%---------------------------------------------------------------------------------------
\section{Object orientation}{
We follow an object-oriented programming approach in this package and
introduce distribution objects. There are mainly four reasons for that:
\itemize{
  \item Unlike most distributions
        the GH distribution has quite a few parameters which have to fulfill some consistency
        requirements. Consistency checks can be performed uniquely when an object is initialized.
       
  \item Once initialized the common functions belonging
        to a distribution can be called conveniently by passing the distribution object. A repeated input of the
        parameters is avoided.
        
  \item Distributions returned from fitting procedures
        can be directly passed to, e.g., the density function since fitted distribution objects 
        add information to the distribution object and consequently inherit from the class of 
        the distribution object.
        
  \item Generic method dispatching can be used to provide a uniform interface to, e.g., 
        simulate random variates of a specific distribution like \code{rand(n, distribution.object}.  
        Additionally, one can take advantage of generic programming since \R provides virtual 
        classes and some forms of polymorphism.}
}
}
\author{
Wolfgang Breymann, David Lüthi \cr

Institute of Data Analyses and Process Design (\url{http://www.idp.zhwin.ch})  \cr

Maintainer: David Lüthi <david.luethi@zhwin.ch>
}
\references{
\emph{Quantitative Risk Management: Concepts, Techniques and Tools} by Alexander J. McNeil, Rüdiger
Frey and Paul Embrechts \cr
Princeton Press, 2005 \cr   

\emph{S-Plus and \R Library for Quantitative Risk Management QRMlib} 
by Alexander J. McNeil (2005) and Scott Ulman (\R-port) (2007)\cr
\url{http://www.math.ethz.ch/~mcneil/book/QRMlib.html} and \code{QRMlib}\cr
}

\keyword{package}
\keyword{hplot}
\keyword{multivariate}
\keyword{iteration}
\keyword{optimize}
\keyword{distribution}
\keyword{models}
\keyword{datagen}

