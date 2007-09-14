"dghyp" <- function(x, object = ghyp(), logvalue = F)
{
  test.ghyp(object, case = "ghyp")
  x <- check.data(x, case = if(object@dimension>1) "mv" else "uv",
                  na.rm = FALSE, fit = FALSE, dim = object@dimension)
  if(object@dimension == 1){
    return(unname(internal.dghyp(as.vector(x), lambda = object@lambda, chi = object@chi, 
                                 psi = object@psi, mu = object@mu, sigma = object@sigma, 
                                 gamma = object@gamma, logvalue = logvalue)))
  }else{
    return(unname(internal.dghypmv(x, object@lambda, object@chi, object@psi,
                                   object@mu, object@sigma, object@gamma, logvalue)))
  }
}

