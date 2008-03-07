"t.transform" <- function(lambda){
  return(-1 - exp(lambda))
}

"inv.t.transform" <- function(lambda.transf){
  return(log(-1 - lambda.transf))
}
