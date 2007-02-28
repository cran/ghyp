"show.ghypmv" <- function(object){

  printf.format <- "%.6f"

  cat("Object of class 'ghypmv' (Multivariate Generalized Hyperbolic)\n\n")
  cat("Model:\n")
  cat(object@model,"\n")
  if(length(grep("Student-t",object@model))==1){
    # Student-t
    cat("\nMixing parameter:\n")
    cat("      nu  (alpha.bar)\n")
    cat(sprintf(printf.format,-2*object@lambda),"        (0)\n",sep="  ")
  }else if(length(grep("Generalized",object@model))==1){
    # ghyp
    cat("\nMixing parameters (lambda / alpha.bar):\n")
    cat("   lambda  alpha.bar\n")
    cat(sprintf(printf.format,object@lambda),
        sprintf(printf.format,object@alpha.bar),"\n",sep="   ")
  }else if(length(grep("Inverse",object@model))==1){
    # NIG
    cat("\nMixing parameters:\n")
    cat("  (lambda)  alpha.bar\n")
    cat("    (-0.5) ",sprintf(printf.format,object@alpha.bar),"\n",sep="  ")
  }else if(length(grep("Variance",object@model))==1){
    # VG
    cat("\nMixing parameters:\n")
    cat("  lambda  (alpha.bar)\n")
    cat(sprintf(printf.format,object@lambda),"        (0)\n",sep="  ")
  }else {
    # hyp
    cat("\nMixing parameters:\n")
    cat("  (lambda)  alpha.bar\n")
    cat("     (",sprintf("%.1f",object@lambda),")   ",
        sprintf(printf.format,object@alpha.bar),"\n",sep="")
  }

  cat("\nmu:\n")
  print(object@mu)
  cat("\nsigma:\n")
  print(object@sigma)
  cat("\ngamma:\n")
  print(object@gamma)


  if(is.null(object@data)|all(object@data==0)){
    cat("\n\nSlot 'data' is NULL.\n")
  }else{
##    if(!is.null(colnames(object@data))){
##      cat("\nColumn names of the data matrix:\n")
##      cat(paste(colnames(object@data),collapse=", "),"\n")
##    }
    cat("\nData matrix dimension:\n")
    cat(dim(object@data),"\n")

  }
}

setMethod("show", signature(object="ghypmv"),show.ghypmv)
