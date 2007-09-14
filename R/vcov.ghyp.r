"vcov.ghyp" <- function(object){
  if(object@dimension == 1){
    return(as.vector(object@variance))
  }else{
    return(object@variance)
  }
}

setMethod("vcov", signature(object = "ghyp"), vcov.ghyp)
