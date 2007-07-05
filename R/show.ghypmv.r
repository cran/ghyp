"show.ghypmv" <- function(object){

  printf.format <- "%.6f"

  format.name <- function(value.string,param.name){
    empty.string <- paste(rep(" ",nchar(value.string)),collapse="",sep="")
    return(paste(substr(empty.string,1,nchar(empty.string)-nchar(param.name)),
                 param.name,sep=""))
  }

  cat("Object of class 'ghypmv' (Multivariate Generalized Hyperbolic)\n\n")
  cat("Model:\n")
  cat(object@model,"\n")
  cat("\nMixing parameter:\n")
  if(length(grep("Student-t",object@model))==1){
    # Student-t
    cat("      nu  (alpha.bar)\n")
    cat(sprintf(printf.format,-2*object@lambda),"        (0)\n",sep="  ")
  }else if(length(grep("Variance",object@model))==1){
    # VG
    lambda.value <- sprintf(printf.format,object@lambda)
    lambda.string <- format.name(lambda.value,"lambda")
    if(abs(Egig(object@lambda, object@chi, object@psi,func="x")-1)<.Machine$double.eps ^ 0.5){
      # when alpha.bar parametrization is used
      alpha.bar.value <- "        (0)"
      alpha.bar.string <- format.name(alpha.bar.value,"(alpha.bar)")
      cat(lambda.string,alpha.bar.string,"\n",sep="  ")
      cat(lambda.value,alpha.bar.value,"\n",sep="  ")
    }else{
      chi.value <- "        (0)"
      chi.string <- format.name(chi.value,"(chi)")
      psi.value <- sprintf(printf.format,object@psi)
      psi.string <- format.name(psi.value,"psi")      
      cat(lambda.string,chi.string,psi.string,"\n",sep="  ")
      cat(lambda.value,chi.value,psi.value,"\n",sep="  ")
    }
  }else{
    if(length(grep("Generalized",object@model))==1){
      # ghyp
      lambda.value <- sprintf(printf.format,object@lambda)
      lambda.string <- format.name(lambda.value,"lambda")
    }else if(length(grep("Inverse",object@model))==1){
      # NIG
      lambda.value <- "  (-0.5)"
      lambda.string <- format.name(lambda.value,"(lambda)")
    }else{
      # hyp
      lambda.value <- paste("   (",sprintf("%.1f",object@lambda),")",sep="")
      lambda.string <- format.name(lambda.value,"(lambda)")
    }
    if(abs(Egig(object@lambda, object@chi, object@psi,func="x")-1)<.Machine$double.eps ^ 0.5){
      # when alpha.bar parametrization is used
      alpha.bar.value <- sprintf(printf.format,object@alpha.bar)
      alpha.bar.string <- format.name(alpha.bar.value,"alpha.bar")
      cat(lambda.string,alpha.bar.string,"\n",sep="  ")
      cat(lambda.value,alpha.bar.value,"\n",sep="  ")
    }else{
      chi.value <- sprintf(printf.format,object@chi)
      chi.string <- format.name(chi.value,"chi")
      psi.value <- sprintf(printf.format,object@psi)
      psi.string <- format.name(psi.value,"psi")      
      cat(lambda.string,chi.string,psi.string,"\n",sep="  ")
      cat(lambda.value,chi.value,psi.value,"\n",sep="  ")
    }
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
    cat("\nData matrix dimension:\n")
    cat(dim(object@data),"\n")

  }
}

setMethod("show", signature(object="ghypmv"),show.ghypmv)
