"summary.mle.ghyp" <- function(object)
{
  if(!object@converged){
     cat("Warning: fitting procedure did not converge!\n\n")
  }
  show.ghyp(object)

  cat("\nCall:\n", deparse(object@call), "\n\n", sep = "")
  
  cat("Optimization information:\n")
  cat("log-Likelihood:               ", logLik(object), "\n")
  cat("AIC:                          ", AIC(object), "\n")  

  nbr.fitted.params <- unname(sum(object@fitted.params[c("alpha.bar","lambda")]) + 
                                  object@dimension * sum(object@fitted.params[c("mu", "gamma")]) +
                                  object@dimension/2 * (object@dimension + 1) * 
                                  object@fitted.params[c("sigma")])
  names.fitted.param <- paste(names(object@fitted.params[object@fitted.params == T]),collapse=", ")
  
  cat("Fitted parameters:             ", names.fitted.param, ";  (Number: ", nbr.fitted.params, ")\n", sep = "")     
  cat("Number of iterations:         ", object@n.iter, "\n")
  cat("Converged:                    ", object@converged, "\n")
  if(!object@converged){
    cat("Error code:                   ", object@error.code, "\n")
    cat("Error message:                ", object@error.message, "\n")
  }
}

setMethod("summary", signature(object = "mle.ghyp"), summary.mle.ghyp)


