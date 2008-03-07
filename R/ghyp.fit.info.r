"ghyp.fit.info" <- function(object)
{
  if(!is(object, "mle.ghyp")){
    stop("Object is not of class 'mle.ghyp'!")
  }
  if(is.univariate(object)){
    return(list(logLikelihood = object@llh,
                aic = object@aic,
                fitted.params = object@fitted.params,    
                converged = object@converged,
                n.iter = object@n.iter,
                error.code = object@error.code,
                error.message = object@error.message,
                parameter.variance = object@parameter.variance))
                
  }else{
    return(list(logLikelihood = object@llh,
                aic = object@aic,
                fitted.params = object@fitted.params,
                converged = object@converged,
                n.iter = object@n.iter,
                error.code = object@error.code,
                error.message = object@error.message))

  }
}
