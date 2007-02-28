"ghyp.params" <- function(object,type=c("chipsi","alpha.bar"))
{
  test.class.ghyp(object,case="ghypbase")
  type <- match.arg(type)
  if(type=="chipsi"){
    param.list <- list(lambda=object@lambda,
                       chi=object@chi,
                       psi=object@psi,
                       mu=object@mu,
                       sigma=object@sigma,
                       gamma=object@gamma)
  }else if(type=="alpha.bar"){
    param.list <- list(lambda=object@lambda,
                       alpha.bar=object@alpha.bar,
                       mu=object@mu,
                       sigma=object@sigma,
                       gamma=object@gamma)
  }
  return(param.list)
}