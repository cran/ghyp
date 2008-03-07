"internal.dghypmv" <- function(x, lambda, chi, psi, mu, sigma, gamma, logvalue = FALSE)
{
  ## Density of a multivariate generalized hyperbolic distribution.
  ## Covers all special cases as well.
  d <- dim(x)[2]
  n <- dim(x)[1]
  det.sigma <- det(sigma)
  inv.sigma <- solve(sigma)
  Q <- mahalanobis(x, mu, inv.sigma, inverted = TRUE)

  if (sum(abs(gamma)) == 0){
    symm <- TRUE
    skewness.scaled <- 0
    skewness.norm <- 0
  } else {
    symm <- FALSE
    skewness.scaled <- as.vector((as.matrix(x) - matrix(mu, nrow = n, ncol = d, byrow = T)) %*% 
                      (inv.sigma %*% gamma))
    skewness.norm <- t(gamma) %*% inv.sigma %*% gamma
  }
  out <- NA
  if (psi == 0){
    lambda.min.d.2 <- lambda - d / 2
    if(symm){
    # Symmetric Student-t
     
      interm <- chi + Q

      log.const.top <- -lambda * log(chi) + lgamma(-lambda.min.d.2)
      log.const.bottom <- d / 2 * log(pi) + 0.5 * log(det.sigma) + lgamma(-lambda)
      log.top <- lambda.min.d.2 * log(interm)

      out <- log.const.top + log.top - log.const.bottom
            
    }else{
    # Asymmetric Student-t
      interm <- sqrt((chi + Q) * skewness.norm)
      
      log.const.top <- -lambda * log(chi) - lambda.min.d.2 * log(skewness.norm)
      log.const.bottom <- d / 2 * log(2 * pi) + 0.5 * log(det.sigma) + lgamma(-lambda) - (lambda + 1) * log(2)

      log.top <- besselM3(lambda.min.d.2, interm, logvalue = TRUE) + skewness.scaled
      log.bottom <- -lambda.min.d.2 * log(interm)

      out <- log.const.top + log.top - log.const.bottom - log.bottom

    }
  }
  else if (psi > 0){
    if (chi > 0){
    # ghyp, hyp and NIG (symmetric and asymmetric)
      log.top <-
      besselM3((lambda - d/2), sqrt((psi + skewness.norm) * (chi + Q)), logvalue = T) + skewness.scaled
      log.bottom <- (d/2 - lambda) * log(sqrt((psi + skewness.norm) * (chi + Q)))
      log.const.top <- -lambda/2 * log(psi * chi) + (d/2) * log(psi) +
                        (d/2 - lambda ) * log(1 + skewness.norm/psi)
      log.const.bottom <- d/2 * log(2 * pi) +
                          besselM3(lambda, sqrt(chi * psi), logvalue = T) + 0.5 * log(det.sigma)
      out <- log.const.top + log.top - log.const.bottom - log.bottom
    }
    else if (chi == 0){
    # Variance gamma (symmetric and asymmetric)
      eps <- .Machine$double.eps
      # Observations that are close to mu were kept at a minimum magnitude
      if(any(Q < eps, na.rm = TRUE)){
        # If lambda == 0.5 * dimension, there is another singularity.
        if(abs(lambda - 0.5 * d) < eps){
          stop("Unhandled singularity: Some standardized observations are close to 0 (< ",
               eps, ") and lambda is close to 0.5 * dimension!\n")
        }else{
          Q[Q < eps] <- eps
          Q[Q == 0] <- eps
          warning("Singularity: Some standardized observations are close to 0 (< ",sprintf("% .6E", eps),")!\n", 
                  "Observations set to ",sprintf("% .6E", eps),".\n",immediate. = TRUE)
        }                         
      }
      log.top <- besselM3((lambda - d/2), sqrt((psi + skewness.norm) * (chi + Q)), logvalue = T) + skewness.scaled
      log.bottom <- (d/2 - lambda) * log(sqrt((psi + skewness.norm) * (chi + Q)))
      log.const.top <- d * log(psi)/2 + (1 - lambda) * log(2) +
                       (d/2 - lambda) * log(1 + skewness.norm/psi)
      log.const.bottom <- (d/2) * log(2 * pi) + lgamma(lambda) + 0.5 * log(det.sigma)
      out <- log.const.top + log.top - log.const.bottom - log.bottom
    }else{
      out <- NA
    }
  }
  if (!logvalue){ 
    out <- exp(out)
  }
  return(out)
}
