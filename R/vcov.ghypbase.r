"vcov.ghypbase" <- function(object){
  return(object@variance)
}
setMethod("vcov", signature(object="ghypbase"),vcov.ghypbase)
