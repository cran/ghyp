"show.mle.ghyp" <- function(object)
{
  if(!object@converged && !is.na(object@n.iter)){
     cat("Warning: fitting procedure did not converge!\n\n")
  }else if(!object@converged && is.na(object@n.iter)){
     cat("Error: fitting procedure crashed! Use 'summary' to see the message!\n\n")  
  }
 
  callNextMethod()
  
  cat("\nlog-likelihood:\n", logLik(object), "\n\n", sep = "")
  
  cat("\nCall:\n", deparse(object@call), "\n\n", sep = "")
}

setMethod("show", signature(object = "mle.ghyp"), show.mle.ghyp)
