"is.symmetric.t" <- function(object){
  return(ghyp.name(object, abbr = FALSE, skew.attr = TRUE) == "Symmetric Student-t")
}
