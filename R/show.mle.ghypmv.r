"show.mle.ghypmv" <- function(object){
  callNextMethod()
  cat("\n\nOptimization information:\n\n")
  cat("log-Likelihood:           ",object@llh,"\n")
  cat("Number of iterations:     ",object@n.iter,"\n")
  cat("Converged:                ",object@converged,"\n")
  cat("Error code:               ",object@error.code,"\n")
  cat("Error message:            ",object@error.message,"\n")
}

setMethod("show", signature(object="mle.ghypmv"),show.mle.ghypmv)
