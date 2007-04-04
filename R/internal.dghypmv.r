"internal.dghypmv" <- function(x, lambda, chi, psi, mu, sigma , gamma ,logvalue=F)
{
  ## Density of a multivariate generalized hyperbolic distribution.
  ## Covers all special cases as well.
  d <- dim(x)[2]
  n <- dim(x)[1]
  det.sigma <- det(sigma)
  inv.sigma <- solve(sigma)
  Q <- mahalanobis(x, mu, inv.sigma, inverted = TRUE)

  if (sum(abs(gamma))==0){
    symm <- TRUE
    tilt <- 0
    Offset <- 0
  } else {
    symm <- FALSE
    tilt <- as.vector((as.matrix(x)-matrix(mu,nrow=n,ncol=d,byrow=T)) %*% (inv.sigma %*% gamma))
    Offset <- t(gamma) %*% inv.sigma %*% gamma
  }
  out <- NA
  if (psi==0){
    nu <- 2*abs(lambda)
    if(symm){
    # Symmetric student-t
      log.const.top <- lgamma((nu + d)/2)
      log.const.bottom <- lgamma(nu/2) + d/2*log((nu-2)*pi) + 0.5 *log(det.sigma)
      log.top <- -(nu + d)/2 * log( 1 + Q/(nu-2) )
      out <- log.const.top + log.top - log.const.bottom
    }else{
    # Asymmetric student-t
      interm <- sqrt((nu+Q -2)*Offset)
      log.top <- besselM3((nu+d)/2,interm,logvalue=T) + tilt
      log.const.top <- log(2)+(nu+d)/4*(log(Offset)-log(Q+nu-2)) +(nu/2)*log(nu/2-1)
      log.const.bottom <- (d/2)*log(2*pi) + 0.5*log(det.sigma) + lgamma( nu/2 )
      out <- log.const.top + log.top - log.const.bottom
    }
  }
  else if (psi >0){
    if (chi>0){
    # ghyp, hyp and NIG (symmetric and asymmetric)
      log.top <-
      besselM3((lambda - d/2), sqrt((psi+Offset)*(chi + Q)),logvalue=T) + tilt
      log.bottom <- (d/2-lambda)*log(sqrt((psi+Offset)*(chi + Q)))
      log.const.top <- (-lambda/2)*log(psi*chi) + (d/2)*log(psi) +
        ( d/2-lambda ) * log( 1+Offset/psi )
      log.const.bottom <- (d/2)*log(2 * pi) +
        besselM3(lambda, sqrt(chi * psi),logvalue=T) + 0.5*log(det.sigma)
      out <- log.const.top + log.top - log.const.bottom - log.bottom
    }
    else if (chi==0){
    # Variance gamma (symmetric and asymmetric)
      eps <- .Machine$double.eps
      # Observations that are close to mu were kept at a minimum magnitude
      if(any(abs(Q)<eps)){
        # If lambda == 0.5 * dimension, there is another singularity.
        if(abs(lambda-0.5*d)<eps){
          stop("Unhandled singularity: Some observations are close to 0 (< ",eps
          , ") and lambda is close to 0.5 * dimension!\n")
        }else{
          Q[abs(Q)<eps] <- sign(Q[abs(Q)<eps])* eps
          Q[Q==0] <- eps
          warning("Some observations are close to 0 (< ",eps,")!\n", immediate. = TRUE)
        }
      }
      log.top <-
        besselM3((lambda - d/2), sqrt((psi+Offset)*(chi + Q)),logvalue=T) + tilt
      log.bottom <- (d/2-lambda)*log(sqrt((psi+Offset)*(chi + Q)))
      log.const.top <- d*log(psi)/2 + (1-lambda)*log(2) +
        (d/2-lambda)*log(1+Offset/psi)
      log.const.bottom <- (d/2)*log(2 * pi) +lgamma(lambda) +
        0.5*log(det.sigma)
      out <- log.const.top + log.top - log.const.bottom - log.bottom
    }
    else out <- NA
  }
  if (!(logvalue)) out <- exp(out)
  out
}
