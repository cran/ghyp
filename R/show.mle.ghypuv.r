"show.mle.ghypuv" <- function(object){
  callNextMethod()
  cat("\n\nOptimization information:\n\n")
  cat("log-Likelihood:           ",object@llh,"\n")
  cat("Number of iterations:     ",object@n.iter,"\n")
  cat("Converged:                ",object@converged,"\n")
  cat("Error code:               ",object@error.code,"\n")
  cat("Error message:            ",object@error.message,"\n")
  cat("Parameter variance:\n")
  print(object@parameter.variance)

}

setMethod("show", signature(object="mle.ghypuv"),show.mle.ghypuv)
