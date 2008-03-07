"dghyp" <- function(x, object = ghyp(), logvalue = FALSE)
{
  test.ghyp(object, case = "ghyp")
  x <- check.data(x, case = if(object@dimension > 1) "mv" else "uv",
                  na.rm = FALSE, fit = FALSE, dim = object@dimension)
  
  if(is.univariate(object)){
  
    d.raw <- rep(NA, length(x))
    d.raw[x == Inf | x == -Inf] <- 0
    d.raw[is.nan(x)] <- NaN
  
    if(is.gaussian(object)){ 
      d.finite <- dnorm(as.vector(x[is.finite(x)]), mean = object@mu, sd = object@sigma, log = logvalue)  
    }else{
      d.finite <- unname(internal.dghyp(as.vector(x[is.finite(x)]), lambda = object@lambda, chi = object@chi, 
                                        psi = object@psi, mu = object@mu, sigma = object@sigma, 
                                        gamma = object@gamma, logvalue = logvalue))
    }
    d.raw[is.finite(x)] <- d.finite
    
    return(d.raw)
  }else{
    if(is.gaussian(object)){ 
      d <- object@dimension
      log.const.bottom <- d / 2 * log(2 * pi) + 0.5 * log(det(object@sigma))
      log.top <- -0.5 * mahalanobis(x, center = object@mu, cov = object@sigma)
      if(logvalue){
        return(log.top - log.const.bottom)      
      }else{
        return(exp(log.top - log.const.bottom))
      }
    }else{
      return(unname(as.vector(internal.dghypmv(x, lambda = object@lambda, chi = object@chi, 
                                               psi = object@psi, mu = object@mu, sigma = object@sigma, 
                                               gamma = object@gamma, logvalue = logvalue))))
    }

  }
}

