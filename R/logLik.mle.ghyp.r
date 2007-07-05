"logLik.mle.ghyp" <- function(object, ...){
  ghyp.models <- list(...)

  test.class.mle.ghyp <- function(x){
    if(!is(x, "mle.ghypuv") & !is(x, "mle.ghypmv")){
      stop("Some of the objects are not of class 'mle.ghypuv'",
           " nor of class 'mle.ghypmv'!")
    }
  }

  sapply(ghyp.models,test.class.mle.ghyp)

  ghyp.models <- c(object,ghyp.models)
  tmp.logLik <- sapply(ghyp.models,function(z)ghyp.fit.info(z)$logLikelihood)
  return(tmp.logLik)
}

setMethod("logLik", signature(object="mle.ghypmv"),logLik.mle.ghyp)
setMethod("logLik", signature(object="mle.ghypuv"),logLik.mle.ghyp)

