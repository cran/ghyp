"ghyp.moment" <- function(object, order = 3:4, 
                          absolute = FALSE, central = TRUE, ...)
{
  test.ghyp(object, case = "univariate")
  if(!is.vector(order)){
    stop("Argument 'order' must be a vector!")
  }
  if(!all(is.finite(order))){
    stop("Argument 'order' must not contain non-numerical values!")
  }
  if(any(order %% 1 != 0) & !absolute){
    stop("Argument absolute must be 'TRUE' when order is not integer!")
  }
  
  internal.moment <- function(x, lambda, chi, psi, mu, sigma, gamma, tmp.order){
    internal.dghyp(x,lambda = lambda, chi = chi, psi = psi, mu = mu,
                   sigma = sigma, gamma = gamma) * x^tmp.order

  }
  abs.internal.moment <- function(x, lambda, chi, psi, mu, sigma, gamma, tmp.order){

    internal.dghyp(x, lambda = lambda, chi = chi, psi = psi, mu = mu,
                   sigma = sigma, gamma = gamma) * abs(x)^tmp.order
  }

  result <- numeric(length(order))
  if(absolute){
    int.function <- abs.internal.moment
  }else{
    int.function <- internal.moment  
  }

  for (i in 1:length(order)) {
    tmp.result <- try(integrate(int.function, -Inf, Inf, tmp.order = order[i],
                                lambda = object@lambda, chi = object@chi, 
                                psi = object@psi, mu = ifelse(central, 0, object@mu), 
                                sigma = object@sigma, gamma = object@gamma, ...)$value)
    if(class(tmp.result) == "try-error"){
      result[i] <- NA
    }else{
      result[i] <- tmp.result
    }
  }
  return(result)
}
