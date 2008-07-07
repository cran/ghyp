"ghyp.omega" <- function(L, object = ghyp(), ...)
{
  ## Only univariate 'ghyp' objects are allowed
  test.ghyp(object, case = "univariate")
  
  call.part <- function(x, L, obj)
    return((x - L) * dghyp(x, obj))

  put.part <- function(x, L, obj)
    return((L - x) * dghyp(x, obj))
  
  int.num <- numeric(length(L))
  int.denom <- numeric(length(L))
  for(i in 1:length(L)){
##    int.num[i] <- integrate(pghyp, object = object, lower = L[i], upper = Inf, lower.tail = FALSE, ...)$value 
##    int.denom[i] <- integrate(pghyp, object = object, lower = -Inf, upper = L[i], ...)$value
    tmp.num <- try(integrate(call.part, lower = L[i], upper = Inf, obj = object, L = L[i], ...))
    if(class(tmp.num) == "try-error"){
      warning("Integral 'int_L^\\infty (x - L) * dghyp(x, obj) dx' did not converge for L =", L[i], "!")
      int.num[i] <- NA    
    }else{
      int.num[i] <- tmp.num$value
    }
    
    tmp.denom <- try(integrate(put.part, lower = -Inf, upper = L[i], obj = object, L = L[i], ...))
    if(class(tmp.denom) == "try-error"){
      warning("Integral 'int_-\\infty^L (L - x) * dghyp(x, obj) dx' did not converge for L =", L[i], "!")
      int.denom[i] <- NA    
    }else{
      int.denom[i] <- tmp.denom$value
    }    
  }
  return(int.num / int.denom)
}
