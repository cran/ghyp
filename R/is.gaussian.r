"is.gaussian" <- function(object){
  return(ghyp.name(object, abbr = TRUE) == "Gauss"  || (object@psi == Inf && object@chi == Inf))
}
