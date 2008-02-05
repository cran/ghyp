"coef.ghyp" <- function(object, type = c("chipsi", "alpha.bar"))
{
  test.ghyp(object,case="ghyp")

  if(object@dimension == 1){
    sigma <- as.vector(object@sigma)
  }else{
    sigma <- object@sigma
  }

  type <- match.arg(type)
  if(type == "chipsi"){
    param.list <- list(lambda = object@lambda,
                       chi = object@chi,
                       psi = object@psi,
                       mu = object@mu,
                       sigma = sigma,
                       gamma = object@gamma)
  }else if(type == "alpha.bar"){
    param.list <- list(lambda = object@lambda,
                       alpha.bar = object@alpha.bar,
                       mu = object@mu,
                       sigma = sigma,
                       gamma = object@gamma)
  }
  return(param.list)
}

setMethod("coef", signature(object = "ghyp"), coef.ghyp)
setMethod("coefficients", signature(object = "ghyp"), coef.ghyp)

"ghyp.params" <- function(object, type = c("chipsi", "alpha.bar"))
{
  warning("'ghyp.params' is replaced by 'coef' and will be removed in the next release!") 
  coef(object, type = type)
}
