"vcov.ghyp" <- function(object){
  if(is.univariate(object)){
    return(as.vector(object@variance))
  }else{
    return(object@variance)
  }
}

setMethod("vcov", signature(object = "ghyp"), vcov.ghyp)
