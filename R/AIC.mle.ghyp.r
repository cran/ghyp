"AIC.mle.ghyp" <- function(object, ..., k = 2)
{
  ghyp.models <- list(...)
  ## Internal function; test whether all objects are of type mle.ghyp
  test.class.mle.ghyp <- function(x){
    if(!is(x, "mle.ghyp")){
      stop("Some of the objects are not of class 'mle.ghyp'!")
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

setMethod("AIC", signature(object="mle.ghyp"),AIC.mle.ghyp)

