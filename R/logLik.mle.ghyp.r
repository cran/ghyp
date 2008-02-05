"logLik.mle.ghyp" <- function(object, ...)
{
  ghyp.models <- list(...)
  ## Internal function; test whether all objects are of type mle.ghyp
  test.class.mle.ghyp <- function(x){
    if(!is(x, "mle.ghyp")){
      stop("Some of the objects are not of class 'mle.ghyp'!")
    }
  }

  sapply(ghyp.models, test.class.mle.ghyp)

  ghyp.models <- c(object, ghyp.models)
  tmp.logLik <- sapply(ghyp.models, function(z)ghyp.fit.info(z)$logLikelihood)
  return(tmp.logLik)
}

setMethod("logLik", signature(object = "mle.ghyp"), logLik.mle.ghyp)

