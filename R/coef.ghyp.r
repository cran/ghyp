"coef.ghyp" <- function(object, type = c("chi.psi", "alpha.bar", "alpha.delta"))
{
  test.ghyp(object, case = "ghyp")

  if(is.univariate(object)){
    sigma <- as.vector(object@sigma)
  }else{
    sigma <- object@sigma
  }

  if(is.gaussian(object)){
    return(list(mu = object@mu, sigma = sigma))
  }
  
  if(missing(type)){
    type <- object@parametrization 
  }else{
    type <- match.arg(type)
  }
  if(type == "chi.psi"){
    param.list <- list(lambda = object@lambda,
                       chi = object@chi,
                       psi = object@psi,
                       mu = object@mu,
                       sigma = sigma,
                       gamma = object@gamma)
  }else if(type == "alpha.bar"){
    if(object@parametrization == "chi.psi"){
      stop("Transformation from 'chi.psi' to 'alpha.bar' parametrization not implemented!")
    }
    if(object@parametrization == "alpha.beta"){
      stop("Transformation from 'alpha.delta' to 'alpha.bar' parametrization not implemented!")
    }
    param.list <- list(lambda = object@lambda,
                       alpha.bar = object@alpha.bar,
                       mu = object@mu,
                       sigma = sigma,
                       gamma = object@gamma)                      
  }else if(type == "alpha.delta"){
    if(is.univariate(object)){   #Univariate
      sigma <- as.numeric(object@sigma)
      alpha <- sqrt(1/sigma^2 * (object@psi + object@gamma^2/sigma^2))
      beta <- object@gamma / sigma^2
      delta <- sqrt(object@chi * sigma^2)
      param.list <- list(lambda = object@lambda, alpha = alpha, delta = delta, beta = beta, 
                         mu = object@mu)
    }else{ # Multivariate
      dimension <- object@dimension 
      inv.sigma <- solve(object@sigma)
      det.sigma <- det(object@sigma)
      alpha <- sqrt(as.numeric(det.sigma^(-1/dimension) * 
                    (object@psi + object@gamma %*% inv.sigma %*% object@gamma)))
      beta <- as.numeric(inv.sigma %*% object@gamma) 
      delta <- sqrt(object@chi * det.sigma^(1/dimension))
      Delta <- det.sigma^(-1/dimension) * object@sigma
      param.list <- list(lambda = object@lambda, alpha = alpha, delta = delta, beta = beta, 
                         mu = object@mu, Delta = Delta)
    }                   
  }
  return(param.list)
}

setMethod("coef", signature(object = "ghyp"), coef.ghyp)
setMethod("coefficients", signature(object = "ghyp"), coef.ghyp)
