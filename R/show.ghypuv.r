"show.ghypuv" <- function(object){
  printf.format <- "%.7f"
  cat("Object of class 'ghypuv' (Univariate Generalized Hyperbolic)\n\n")
  cat("Model:\n")
  cat(object@model,"\n")
  cat("\nParameters:\n")

  format.name <- function(value.string,param.name){
    empty.string <- paste(rep(" ",nchar(value.string)),collapse="",sep="")
    return(paste(substr(empty.string,1,nchar(empty.string)-nchar(param.name)),
                 param.name,sep=""))
  }
  if(object@gamma==0){
    gamma.value <- "        (0)"
  }else{
    gamma.value <- sprintf(printf.format,object@gamma)
  }
  mu.value <- sprintf(printf.format,object@mu)
  sigma.value <- sprintf(printf.format,object@sigma)
  mu.string <- format.name(mu.value,"mu")
  sigma.string <- format.name(sigma.value,"sigma")
  gamma.string <- format.name(gamma.value,"gamma")

  if(length(grep("Student-t",object@model))==1){
    # Student-t
    nu.value <- sprintf(printf.format,-2*object@lambda)
    nu.string <- format.name(nu.value,"nu")
    alpha.bar.value <- "        (0)"
    alpha.bar.string <- format.name(alpha.bar.value,"(alpha.bar)")
    cat(nu.string,alpha.bar.string,mu.string,sigma.string,gamma.string,"\n",sep="  ")
    cat(nu.value,alpha.bar.value,mu.value,sigma.value,gamma.value,"\n",sep="  ")
  }else if(length(grep("Variance",object@model))==1){
    # VG
    lambda.value <- sprintf(printf.format,object@lambda)
    lambda.string <- format.name(lambda.value,"lambda")
    if(abs(ghyp.moments(object)$e.gig-1)<.Machine$double.eps ^ 0.5){
      # when alpha.bar parametrization is used
      alpha.bar.value <- "        (0)"
      alpha.bar.string <- format.name(alpha.bar.value,"(alpha.bar)")
      cat(lambda.string,alpha.bar.string,mu.string,sigma.string,gamma.string,"\n",sep="  ")
      cat(lambda.value,alpha.bar.value,mu.value,sigma.value,gamma.value,"\n",sep="  ")
    }else{
      chi.value <- "        (0)"
      chi.string <- format.name(chi.value,"(chi)")
      psi.value <- sprintf(printf.format,object@psi)
      psi.string <- format.name(psi.value,"psi")      
      cat(lambda.string,chi.string,psi.string,mu.string,sigma.string,gamma.string,"\n",sep="  ")
      cat(lambda.value,chi.value,psi.value,mu.value,sigma.value,gamma.value,"\n",sep="  ")
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
    if(abs(ghyp.moments(object)$e.gig-1)<.Machine$double.eps ^ 0.5){
      # when alpha.bar parametrization is used
      alpha.bar.value <- sprintf(printf.format,object@alpha.bar)
      alpha.bar.string <- format.name(alpha.bar.value,"alpha.bar")
      cat(lambda.string,alpha.bar.string,mu.string,sigma.string,gamma.string,"\n",sep="  ")
      cat(lambda.value,alpha.bar.value,mu.value,sigma.value,gamma.value,"\n",sep="  ")
    }else{
      chi.value <- sprintf(printf.format,object@chi)
      chi.string <- format.name(chi.value,"chi")
      psi.value <- sprintf(printf.format,object@psi)
      psi.string <- format.name(psi.value,"psi")      
      cat(lambda.string,chi.string,psi.string,mu.string,sigma.string,gamma.string,"\n",sep="  ")
      cat(lambda.value,chi.value,psi.value,mu.value,sigma.value,gamma.value,"\n",sep="  ")
    }
  }
  if(is.null(object@data)|all(object@data==0)){
    cat("\n\nSlot 'data' is NULL.\n")
  }else{
    if(!is.null(colnames(object@data))){
      cat("\nColumn names of the data vector:\n")
      cat(colnames(object@data),"\n")
    }
    cat("\nLength of data vector :\n")
    cat(length(object@data),"\n")

  }
}


setMethod("show", signature(object="ghypuv"),show.ghypuv)
