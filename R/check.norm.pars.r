"check.norm.pars" <- function(mu, sigma, gamma, dimension){
  if(length(mu) != dimension){
    stop("Parameter 'mu' must be of length ", dimension, "!")
  }
  if(length(gamma) != dimension){
    stop("Parameter 'gamma' must be of length ", dimension, "!")
  }
  if(dimension > 1){ # Multivariate case
    if(!is.matrix(sigma)){
      stop("'sigma' must be a quadratic matrix with dimension ",
           dimension, " x ", dimension, "!")    
    }
    if(nrow(sigma) != dimension | ncol(sigma) != dimension){
      stop("Matrix 'sigma' must be quadratic with dimension ",
           dimension, " x ", dimension, "!")
    }
  }else{
    if(length(sigma) != dimension){
      stop("Parameter 'sigma' must be a scalar!")
    }
  }  
}
