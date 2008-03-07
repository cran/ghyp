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
    skewness.scaled <- 0
    skewness.norm <- 0
  } else {
    symm <- FALSE
    skewness.scaled <- (x - mu) * (inv.sigma * gamma)
    skewness.norm <- gamma^2 * inv.sigma
  }
  out <- NA
  if (psi == 0){
    if(symm){
    # Symmetric Student-t
      nu <- -2 * lambda
      sigma.t <- sqrt((nu - 2) / nu) * sigma
      out <- dt((x - mu) / sigma.t, df = nu, log = TRUE) - log(sigma.t)
    }else{
    # Asymmetric Student-t
      interm <- sqrt((chi + Q) * skewness.norm)
      lambda.min.0.5 <- lambda - 0.5
      
      log.const.top <- -lambda * log(chi) - lambda.min.0.5 * log(skewness.norm)
      log.const.bottom <- 0.5 * log(2 * pi) + log(sigma) + lgamma(-lambda) - (lambda + 1) * log(2)

      log.top <- besselM3(lambda.min.0.5, interm, logvalue = TRUE) + skewness.scaled
      log.bottom <- -lambda.min.0.5 * log(interm)

      out <- log.const.top + log.top - log.const.bottom - log.bottom
    }
  }else if(psi > 0){
    if(chi > 0){
    # ghyp, hyp and NIG (symmetric and asymmetric)
      log.top <- besselM3((lambda - 0.5), sqrt((psi + skewness.norm) * (chi + Q)),logvalue = TRUE) + skewness.scaled
      log.bottom <- (0.5 - lambda) * log(sqrt((psi + skewness.norm) * (chi + Q)))
      log.const.top <- -lambda/2 * log(psi * chi) + 0.5 * log(psi) +
                       (0.5 - lambda ) * log(1 + skewness.norm/psi)
      log.const.bottom <- 0.5 * log(2 * pi) +
                          besselM3(lambda, sqrt(chi * psi),logvalue = TRUE) + log(sigma)
      out <- log.const.top + log.top - log.const.bottom - log.bottom
    }else if(chi == 0){
    # Variance gamma (symmetric and asymmetric)

    # Density contains singularities for Q -> 0. Three cases depending on
    # lambda are catched:

    # any(Q < eps) & lambda >  0.5: Interpolate with splines
    # any(Q < eps) & lambda <  0.5: Set Q[Q < eps] to eps
    # any(Q < eps) & lambda == 0.5: Set Q[Q < eps] to NA 
      
      #<---------------  Internal function vg.density  ------------------>
      vg.density <- function(Q, skewness.norm, skewness.scaled, lambda, psi, sigma)
      {
        log.top <- besselM3((lambda - 0.5), sqrt((psi + skewness.norm) * Q),logvalue = TRUE) + skewness.scaled
        log.bottom <- (0.5 - lambda) * log(sqrt((psi + skewness.norm) * Q))
  
        log.const.top <- log(psi) * lambda + (1 - lambda) * log(2) +
                         (0.5 - lambda) * log(psi + skewness.norm)

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
            tmp.skewness.scaled <- 0
          }else{
            tmp.skewness.scaled <- skewness.scaled[Q >= eps]
          }
          out <- rep(NA, length(Q))
          out[Q >= eps] <- vg.density(Q[Q >= eps], skewness.norm, tmp.skewness.scaled, lambda, psi, sigma) 
        }else if(lambda > 0.5){ 
          message("Singularity (x-mu)==0: Interpolate with splines.")

          #<----------  Internal function vg.density.singular  -------------->
          vg.density.singular <- function(skewness.norm, lambda, psi, sigma)
          {
            log.const.top <- log(psi) * lambda + (0.5 - lambda) * log(psi + skewness.norm) + 
                             lgamma(lambda - 0.5)
    
            log.const.bottom <- log(2) + 0.5 * log(pi) + lgamma(lambda) + log(sigma)
            return(log.const.top - log.const.bottom)
          }
          #<------  End of internal function vg.density.singular  -----------> 
          
          # Compute observations > eps as usual
          out <- rep(0, length(Q))

          if(gamma == 0){
            tmp.skewness.scaled <- 0
          }else{
            tmp.skewness.scaled <- skewness.scaled[Q >= eps]
          }
          
          out[Q >= eps] <- vg.density(Q[Q >= eps], skewness.norm, tmp.skewness.scaled, lambda, psi, sigma)
          
          # Interpolate all observations < eps
          # x points
          tmp.x <- c(-2, -1, 1, 2) * sqrt(eps) * sigma + mu 
          spline.x <- c(tmp.x[1:2], mu, tmp.x[3:4])
          
          # y points          
          if(gamma == 0){
            tmp.skewness.scaled <- 0
          }else{
            tmp.skewness.scaled <- (tmp.x - mu) * inv.sigma * gamma 
          }
          
          tmp.Q <- ((tmp.x - mu)/sigma)^2
          tmp.density <-  vg.density(tmp.Q, skewness.norm, tmp.skewness.scaled, lambda, psi, sigma)
          
          spline.y <- c(tmp.density[1:2], vg.density.singular(skewness.norm, lambda, psi, sigma), 
                        tmp.density[3:4])

          vg.density.interp <- splinefun(spline.x, spline.y)
          out[Q < eps] <- vg.density.interp(x[Q < eps])          
        }else{
          Q[Q < eps] <- eps
          warning("Singularity: Some standardized observations are close to 0 (< ",
                  sprintf("% .6E", eps), ")! Observations set to ", 
                  sprintf("% .6E", eps), ".", immediate. = TRUE)

          out <- vg.density(Q, skewness.norm, skewness.scaled, lambda, psi, sigma)
        }
      }else{
        out <- vg.density(Q, skewness.norm, skewness.scaled, lambda, psi, sigma)
      }
    }
    else out <- NA
  }
  if (!logvalue){
    out <- exp(out)
  }
  return(out)
}
