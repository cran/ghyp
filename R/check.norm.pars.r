"check.norm.pars" <- function(mu,sigma,gamma){
  if(length(mu)>1){
    if(ncol(sigma)!=length(mu)){
      stop("Dimension mismatch ( ncol(sigma)!=length(mu) )!\n")
    }
    if(length(gamma)!=length(mu)){
      stop("Dimension mismatch ( length(gamma)!=length(mu) )!\n")
    }
    if(any(diag(sigma) <= 0)){
      stop("All elements of sigma must be > 0!")
    }
  }else if(length(mu)==1){
    if(sigma <= 0){
      stop("sigma must be > 0!")
    }
    if(is.matrix(sigma)){
      if(!all(dim(sigma)==1)){
        stop("Sigma must have dimension 1 or length of mu must be bigger than 1.")
      }
    }
    if(length(gamma)!=1){
        stop("Gamma must have a length of 1.")
    }
  }else {
    stop("Invalid input (length(mu)==0)!")
  }
}
