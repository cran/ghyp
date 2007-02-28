"internal.dghyp" <- function(x, lambda=1, chi=1, psi=1, alpha.bar=NULL, mu=1, sigma=1 , gamma=0 ,logvalue=F)
{
  ## Density of a univariate generalized hyperbolic distribution.
  ## Covers all special cases as well.
  if(!is.null(alpha.bar)){
    tmp.abar2chipsi <- abar2chipsi(alpha.bar,lambda)
    chi  <- tmp.abar2chipsi$chi
    psi  <- tmp.abar2chipsi$psi
  }
  Q <- ((x - mu)/sigma)^2
  d <- 1
  n <- length(x)
  inv.sigma <- 1/sigma^2

  if (sum(abs(gamma))==0){
    symm <- TRUE
    tilt <- 0
    Offset <- 0
  } else {
    symm <- FALSE
    tilt <- (x-mu) * (inv.sigma * gamma)
    Offset <- gamma^2 * inv.sigma
  }
  out <- NA
  if (psi==0){
    nu <- 2*abs(lambda)
    if(symm){
    # Symmetric student-t
      log.const.top <- lgamma((nu + 1)/2)
      log.const.bottom <- lgamma(nu/2) + 0.5*log((nu-2)*pi) + log(sigma)
      log.top <- -(nu + 1)/2 * log( 1 + Q/(nu-2) )
      out <- log.const.top + log.top - log.const.bottom
    }else{
    # Asymmetric student-t
      interm <- sqrt((nu+Q -2)*Offset)
      log.top <- besselM3((nu+1)/2,interm,logvalue=T) + tilt
      log.const.top <- log(2)+(nu+1)/4*(log(Offset)-log(Q+nu-2)) +(nu/2)*log(nu/2-1)
      log.const.bottom <- 0.5*log(2*pi) + log(sigma) + lgamma( nu/2 )
      out <- log.const.top + log.top - log.const.bottom
    }
  }
  else if (psi >0){
    if (chi>0){
    # ghyp, hyp and NIG (symmetric and asymmetric)
      log.top <-
      besselM3((lambda - 0.5), sqrt((psi+Offset)*(chi + Q)),logvalue=T) + tilt
      log.bottom <- (0.5-lambda)*log(sqrt((psi+Offset)*(chi + Q)))
      log.const.top <- (-lambda/2)*log(psi*chi) + 0.5*log(psi) +
        ( 0.5-lambda ) * log( 1+Offset/psi )
      log.const.bottom <- 0.5*log(2 * pi) +
        besselM3(lambda, sqrt(chi * psi),logvalue=T) + log(sigma)
      out <- log.const.top + log.top - log.const.bottom - log.bottom
    }
    else if (chi==0){
    # Variance gamma (symmetric and asymmetric)
      eps <- .Machine$double.eps
      # Observations that are close to mu were kept at a minimum magnitude
      if(any(abs(Q)<eps)){
        # If lambda == 0.5 * dimension, there is another singularity.
        if(abs(lambda-0.5*d)<eps){
          stop("Unhandled singularity: Some observations are close to 0 (<",eps,
               ") and lambda is close to 0.5!\n")
        }else{
          Q[abs(Q)<eps] <- sign(Q[abs(Q)<eps])* eps
          Q[Q==0] <- eps
          warning("Some observations are close to 0 (<",eps,")!\n")
        }
      }
      log.top <-
        besselM3((lambda - 0.5), sqrt((psi+Offset)*(chi + Q)),logvalue=T) + tilt
      log.bottom <- (0.5-lambda)*log(sqrt((psi+Offset)*(chi + Q)))
      log.const.top <- log(psi)/2 + (1-lambda)*log(2) +
        (0.5-lambda)*log(1+Offset/psi)
      log.const.bottom <- 0.5*log(2 * pi) +lgamma(lambda) +
        log(sigma)
      out <- log.const.top + log.top - log.const.bottom - log.bottom
    }
    else out <- NA
  }
  if (!(logvalue)) out <- exp(out)
  out
}
