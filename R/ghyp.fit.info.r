"ghyp.fit.info" <- function(object)
{
  if(!is(object, "mle.ghyp")){
    stop("Object is not of class 'mle.ghyp'!")
  }

  if(length(object@trace.pars) == 0){
    trace.pars <- NULL
  }else{
    if(is.univariate(object)){
      trace.pars <- as.data.frame(object@trace.pars)   
    }else{
      trace.pars <- object@trace.pars  
    }
  }
  
  if(is.univariate(object)){
    return(list(logLikelihood = object@llh,
                aic = object@aic,
                fitted.params = object@fitted.params,    
                converged = object@converged,
                n.iter = object@n.iter,
                error.code = object@error.code,
                error.message = object@error.message,
                parameter.variance = object@parameter.variance,
                trace.pars = trace.pars))
                
  }else{
    return(list(logLikelihood = object@llh,
                aic = object@aic,
                fitted.params = object@fitted.params,
                converged = object@converged,
                n.iter = object@n.iter,
                error.code = object@error.code,
                error.message = object@error.message,
                trace.pars = trace.pars))

  }
}
