"besselM3" <- function(lambda = 9/2, x = 2, logvalue = FALSE)
{
  if(all(abs(lambda) == 0.5)){
    if (!logvalue){
      res <- sqrt(pi/(2 * x)) * exp(-x)
    }else{
      res <- 0.5 * log(pi/(2 * x)) - x
    }    
  }else{
    if (!logvalue){
      res <- besselK(x, lambda)
    }else{
      res <- log( besselK(x, lambda, expon.scaled = TRUE) ) - x
    }
  }
  return(res)
}
