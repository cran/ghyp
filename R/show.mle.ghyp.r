"show.mle.ghyp" <- function(object)
{
  if(!object@converged){
     cat("Warning: fitting procedure did not converge!\n\n")
  }
 
  callNextMethod()
  
  cat("\nlog-likelihood:\n", logLik(object), "\n\n", sep = "")
  
  cat("\nCall:\n", deparse(object@call), "\n\n", sep = "")
}

setMethod("show", signature(object = "mle.ghyp"), show.mle.ghyp)
