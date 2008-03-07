"rghyp" <- function(n, object = ghyp())
{
  test.ghyp(object, case = "ghyp")
  if(is.univariate(object)){
    if(is.gaussian(object)){
      return(rnorm(n, mean = object@mu, sd = object@sigma))
    }else if(is.symmetric.t(object)){
      nu <- -2 * coef(object)$lambda
      return(rt(n, df = nu) * sqrt((nu - 2) / nu) * object@sigma + object@mu)    
    }else{
      W <- rgig(n, object@lambda, object@chi, object@psi)
      return(return(object@mu + W * object@gamma + sqrt(W) * object@sigma * rnorm(n)))    
    }
  }else{
    Z <- matrix(rnorm(n * object@dimension), ncol = object@dimension)
    A <- chol(object@sigma, pivot = FALSE)
    if(is.gaussian(object)){
      return(Z %*% A  +  matrix(rep(object@mu, n), ncol = object@dimension, byrow = TRUE))
    }else{
      W <- rgig(n, object@lambda, object@chi, object@psi)
      return(sqrt(W) * (Z %*% A)  + 
             matrix(rep(object@mu, n), ncol = object@dimension, byrow = TRUE) + 
             outer(W, object@gamma))
    }
  }
}
