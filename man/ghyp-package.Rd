\name{ghyp-package}
\alias{ghyp-package}
\docType{package}
\title{
A package on generalized hyperbolic distributions
}
\description{
This package provides all about univariate and multivariate generalized hyperbolic 
distributions. Especially fitting procedures, computation of the density, quantile, probability, 
random variates, expected shortfall and some portfolio optimization and plotting routines. 
In addition the generalized inverse gaussian distribution is contained in this package.
}
\details{
\tabular{ll}{
Package: \tab ghyp\cr
Type: \tab Package\cr
Version: \tab 0.8.3\cr
Date: \tab 2007-02-28\cr
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
}

\strong{Portfolio optimization and utilities:}
\tabular{ll}{
  \code{\link{portfolio.optimize}} \tab     Calculate an optimal portfolio given a multivariate \code{ghyp} distribution\cr
  \code{\link[=mean.ghypbase]{mean}} \tab   Returns the expected value\cr
  \code{\link[=vcov.ghypbase]{vcov}} \tab   Returns the variance in the univariate case or else the variance covariance matrix\cr
  \code{\link{redim}} \tab                  Extract certain dimensions of a multivariate \code{ghyp} distribution\cr
  \code{\link{lin.transf}} \tab             Transform a multivariate generalized hyperbolic distribution\cr
  \code{\link{ghyp.moments}} \tab           Expected value and variance of the \code{ghyp} and the \code{GIG} distribution\cr
  \code{\link{ghyp.params}} \tab            Parameters of a generalized hyperbolic distribution \cr
  \code{\link{ghyp.data}} \tab              Data of a (fitted) generalized hyperbolic distribution\cr
  \code{\link{ghyp.fit.info}} \tab          Information about the fitting procedure\cr      
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
  generalized hyperbolic distribution. Additionally, the above-mentioned packages have
  some restrictions concerning the fitting procedures (e.g. no possibility to keep some
  parameters constant) and the special cases are not (completely) covered (\code{fBasics} covers 
  the hyperbolic and normal inverse gaussian case). \cr
  The package \code{fMultivar} implements
  a fitting routine for multivariate skewed student-t distributions.
}
%---------------------------------------------------------------------------------------
\section{Object orientation}{
We follow an object oriented programming approach in this package and
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
%---------------------------------------------------------------------------------------
%\section{Extensions and upgrading}{
%  This package is intended to be extended with further features like copulae et cetera.
%}
%---------------------------------------------------------------------------------------

\author{
Wolfgang Breymann, David Lüthi \cr

Institute of Data Analyses and Process Design (\url{www.idp.zhwin.ch})  \cr

Maintainer: David Lüthi <david.luethi@zhwin.ch>
}
\references{
Alexander J. McNeil, Rüdiger Frey, Paul Embrechts (2005) \emph{Quantitative
Risk Management, Concepts, Techniques and Tools} \cr   

Alexander J. McNeil(2005), S-Plus Library for Quantitative Risk Management 
\emph{QRMlib}, \url{http://www.math.ethz.ch/~mcneil/book/QRMlib.html}\cr
}

\keyword{package}
\keyword{dplot}
\keyword{hplot}
\keyword{multivariate}
\keyword{distribution}
\keyword{models}
