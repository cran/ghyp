"internal.dghyp" <- function(x, lambda = 1, chi = 1, psi = 1, alpha.bar = NULL, 
                             mu = 1, sigma = 1, gamma = 0, logvalue = FALSE)
{
  sigma <- as.vector(sigma)
  ## Density of a univariate generalized hyperbolic distribution.
  ## Covers all special cases as well.
  if(!is.null(alpha.bar)){
    tmp.abar2chipsi <- abar2chipsi(alpha.bar, lambda)
    chi <- tmp.abar2chipsi$chi
    psi <- tmp.abar2chipsi$psi
  }
  Q <- ((x - mu)/sigma)^2
  d <- 1
  n <- length(x)
  inv.sigma <- 1/sigma^2

  if(gamma == 0){
    symm <- TRUE
    tilt <- 0
    Offset <- 0
  } else {
    symm <- FALSE
    tilt <- (x - mu) * (inv.sigma * gamma)
    Offset <- gamma^2 * inv.sigma
  }
  out <- NA
  if (psi == 0){
    nu <- 2 * abs(lambda)
    if(symm){
    # Symmetric student-t
      log.const.top <- lgamma((nu + 1)/2)
      log.const.bottom <- lgamma(nu/2) + 0.5 * log((nu - 2) * pi) + log(sigma)
      log.top <- -(nu + 1)/2 * log(1 + Q/(nu - 2))
      out <- log.const.top + log.top - log.const.bottom
    }else{
    # Asymmetric student-t
      interm <- sqrt((nu + Q - 2) * Offset)
      log.top <- besselM3((nu + 1)/2, interm, logvalue = T) + tilt
      log.const.top <- log(2) + (nu + 1)/4 * (log(Offset) - 
                       log(Q + nu - 2)) + (nu/2) * log(nu/2 - 1)
      log.const.bottom <- 0.5 * log(2 * pi) + log(sigma) + lgamma(nu/2)
      out <- log.const.top + log.top - log.const.bottom
    }
  }else if(psi > 0){
    if(chi > 0){
    # ghyp, hyp and NIG (symmetric and asymmetric)
      log.top <- besselM3((lambda - 0.5), sqrt((psi + Offset) * (chi + Q)),logvalue = T) + tilt
      log.bottom <- (0.5 - lambda) * log(sqrt((psi + Offset) * (chi + Q)))
      log.const.top <- -lambda/2 * log(psi * chi) + 0.5 * log(psi) +
                       (0.5 - lambda ) * log(1 + Offset/psi)
      log.const.bottom <- 0.5 * log(2 * pi) +
                          besselM3(lambda, sqrt(chi * psi),logvalue = T) + log(sigma)
      out <- log.const.top + log.top - log.const.bottom - log.bottom
    }else if(chi == 0){
    # Variance gamma (symmetric and asymmetric)

    # Density contains singularities for Q -> 0. Three cases depending on
    # lambda are catched:

    # any(Q < eps) & lambda >  0.5: Interpolate with splines
    # any(Q < eps) & lambda <  0.5: Set Q[Q < eps] to eps
    # any(Q < eps) & lambda == 0.5: Set Q[Q < eps] to NA 
      
      #<---------------  Internal function vg.density  ------------------>
      vg.density <- function(Q, Offset, tilt, lambda, psi, sigma)
      {
        log.top <- besselM3((lambda - 0.5), sqrt((psi + Offset) * Q),logvalue = T) + tilt
        log.bottom <- (0.5 - lambda) * log(sqrt((psi + Offset) * Q))
  
        log.const.top <- log(psi) * lambda + (1 - lambda) * log(2) +
                         (0.5 - lambda) * log(psi + Offset)

        log.const.bottom <- 0.5 * log(2 * pi) + lgamma(lambda) + log(sigma)
        return(log.const.top + log.top - log.const.bottom - log.bottom)
      }
      #<-------------  End of internal function vg.density  ------------>      
      
      eps <- .Machine$double.eps
      # Observations that are close to mu were kept at a minimum magnitude
      if(any(Q < eps)){
        # If lambda == 0.5 * dimension, there is another singularity.
        if(lambda == 0.5){
          warning("NA's generated in internal.dghyp: Some standardized observations are close to 0 ",
                  "and lambda is close to 0.5!")
          if(gamma == 0){
            tmp.tilt <- 0
          }else{
            tmp.tilt <- tilt[Q >= eps]
          }
          out <- rep(NA, length(Q))
          out[Q >= eps] <- vg.density(Q[Q >= eps], Offset, tmp.tilt, lambda, psi, sigma) 
        }else if(lambda > 0.5){ 
          message("Singularity (x-mu)==0: Interpolate with splines.")

          #<----------  Internal function vg.density.singular  -------------->
          vg.density.singular <- function(Offset, lambda, psi, sigma)
          {
            log.const.top <- log(psi) * lambda + (0.5 - lambda) * log(psi + Offset) + 
                             lgamma(lambda - 0.5)
    
            log.const.bottom <- log(2) + 0.5 * log(pi) + lgamma(lambda) + log(sigma)
            return(log.const.top - log.const.bottom)
          }
          #<------  End of internal function vg.density.singular  -----------> 
          
          # Compute observations > eps as usual
          out <- rep(0, length(Q))

          if(gamma == 0){
            tmp.tilt <- 0
          }else{
            tmp.tilt <- tilt[Q >= eps]
          }
          
          out[Q >= eps] <- vg.density(Q[Q >= eps], Offset, tmp.tilt, lambda, psi, sigma)
          
          # Interpolate all observations < eps
          # x points
          tmp.x <- c(-2, -1, 1, 2) * sqrt(eps) * sigma + mu 
          spline.x <- c(tmp.x[1:2], mu, tmp.x[3:4])
          
          # y points          
          if(gamma == 0){
            tmp.tilt <- 0
          }else{
            tmp.tilt <- (tmp.x - mu) * inv.sigma * gamma 
          }
          
          tmp.Q <- ((tmp.x - mu)/sigma)^2
          tmp.density <-  vg.density(tmp.Q, Offset, tmp.tilt, lambda, psi, sigma)
          
          spline.y <- c(tmp.density[1:2], vg.density.singular(Offset, lambda, psi, sigma), 
                        tmp.density[3:4])

          vg.density.interp <- splinefun(spline.x, spline.y)
          out[Q < eps] <- vg.density.interp(x[Q < eps])          
        }else{
          Q[Q < eps] <- eps
          warning("Singularity: Some standardized observations are close to 0 (< ",
                  sprintf("% .6E", eps), ")! Observations set to ", 
                  sprintf("% .6E", eps),".", immediate. = TRUE)

          out <- vg.density(Q, Offset, tilt, lambda, psi, sigma)
        }
      }else{
        out <- vg.density(Q, Offset, tilt, lambda, psi, sigma)
      }
    }
    else out <- NA
  }
  if (!logvalue){
    out <- exp(out)
  }
  return(out)
}
