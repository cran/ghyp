"ghyp.fit.info" <- function(object){
  if(is(object, "mle.ghypuv")){
    return(list(logLikelihood = object@llh,
                converged = object@converged,
                n.iter = object@n.iter,
                error.code = object@error.code,
                error.message = object@error.message,
                parameter.variance = object@parameter.variance))
                
  }else if(is(object, "mle.ghypmv")){
    return(list(logLikelihood = object@llh,
                converged = object@converged,
                n.iter = object@n.iter,
                error.code = object@error.code,
                error.message = object@error.message))

  }
  else{
    stop("Object in not of class 'mle.ghypuv' nor of class 'em.ghypmv'!\n")
  }
}
