"rghyp" <- function(n,object=ghyp())
{
  test.ghyp(object, case = "ghyp")
  W <- rgig(n, object@lambda, object@chi, object@psi)
  if(object@dimension == 1){
    return(return(object@mu + W * object@gamma + sqrt(W) * object@sigma * rnorm(n)))
  }else{
    Z <- matrix(rnorm(n * object@dimension), ncol = object@dimension)
    A <- chol(object@sigma, pivot = FALSE)
    return(sqrt(W) * (Z %*% A)  + 
           matrix(rep(object@mu, n), ncol = object@dimension, byrow = T) + 
           outer(W, object@gamma))
  }
}
