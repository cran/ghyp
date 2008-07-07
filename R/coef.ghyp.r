"coef.ghyp" <- function(object, type = c("chi.psi", "alpha.bar", "alpha.delta"))
{
  ## test.ghyp(object, case = "ghyp")

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
    if(is.student.t(object) & object@parametrization == "alpha.bar"){
      param.list <- list(lambda = object@lambda,
                         nu = -2 * object@lambda, 
                         alpha.bar = object@alpha.bar,
                         mu = object@mu,
                         sigma = sigma,
                         gamma = object@gamma)
    }else{  
      if(is.student.t(object) & object@lambda >= -1){
        stop("Transformation to 'alpha.bar' parametrization not possible in case of Student-t distribution with",
             " a degree of freedom <= 2!")
      }      
      k <- Egig(object@lambda, object@chi, object@psi, func = "x")
      if(is.univariate(object)){
        transf.sigma <- sqrt(k) * sigma
      }else{
        transf.sigma <- k * sigma      
      }
      param.list <- list(lambda = object@lambda,
                         alpha.bar = sqrt(object@chi * object@psi),
                         mu = object@mu,
                         sigma = transf.sigma,
                         gamma = k * object@gamma)
    }
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
