"AIC.mle.ghyp" <- function(object, ..., k = 2)
{
  ghyp.models <- list(...)
  ## Internal function; test whether all objects are of type mle.ghypuv or mle.ghypmv
  test.class.mle.ghyp <- function(x){
    if(!is(x, "mle.ghypuv") & !is(x, "mle.ghypmv")){
      stop("Some of the objects are not of class 'mle.ghypuv'", 
           " nor of class 'mle.ghypmv'!")
    }
  }

  ## Internal function; extract the number of fitted parameters  
  nbr.fitted.params <- function(z){
    tmp <- ghyp.fit.info(z)
    opt.pars <- tmp$fitted.params
    if(z@dimension==1){
      return(unname(sum(opt.pars)))
    }else{
      return(unname(sum(opt.pars[c("alpha.bar","lambda")]) + 
                              z@dimension * sum(opt.pars[c("mu","gamma")]) +
                              z@dimension/2 * (z@dimension + 1) * 
                              opt.pars[c("sigma")]))
    }
  }

  sapply(ghyp.models,test.class.mle.ghyp)
  
  ghyp.models <- c(object,ghyp.models)
  
  nbr.fitted <- sapply(ghyp.models,nbr.fitted.params)

  tmp.aic <- -2 * logLik(object,...) + k * nbr.fitted
   
  return(tmp.aic)
}

setMethod("AIC", signature(object="mle.ghypuv"),AIC.mle.ghyp)
setMethod("AIC", signature(object="mle.ghypmv"),AIC.mle.ghyp)
