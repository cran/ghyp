"t.transform" <- function(lambda){
  return(min(-abs(lambda),-1 - sqrt(.Machine$double.eps)))
}
