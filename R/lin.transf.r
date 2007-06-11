"lin.transf" <- function(object,summand,multiplier){
  test.class.ghyp(object,case="ghypbase")
  ## There are 4 cases:
  ## (1) 'summand' is missing: Set 'summand' to 0
  ## (2) 'multiplier' is missing: Set 'multiplier' to the unit matrix 
  ## (3) 'multiplier' and 'summand' are passed: 
  ##     The dimension of 'multiplier' is k x n : 'n' must be the dimension 
  ##                                              ghyp object and 'k' must be
  ##                                              the length of 'summand'
  ## (4) 'multiplier' and 'summand' are missing: -> stop
  if(missing(summand) & missing(multiplier)){
  ## stop if 'summand' and 'multiplier' are missing 
    stop("No arguments submitted!")
  }else if(missing(summand)){
     if(!is.matrix(multiplier)){
       multiplier <- matrix(multiplier,nrow=1)
     }
     if(ncol(multiplier)!= object@dimension){
        stop("Dimension of multiplier must be ",
             "n x ",object@dimension,"!",sep="")
     }
     summand <- rep(0,nrow(multiplier))
  }else if(missing(multiplier)){
     summand <- as.vector(summand)
     if(length(summand)!= object@dimension){
        stop("Dimension of summand must be ",object@dimension,"!",sep="")
     }
     multiplier <- diag(rep(1,object@dimension))  
  }else {
     summand <- as.vector(summand)
     if(!is.matrix(multiplier)){
       multiplier <- matrix(multiplier,nrow=1)
     }
     if(ncol(multiplier) != object@dimension){
        stop("Dimension mimatch: ncol(multiplier) must be equal to the dimension of the object!")
     }
     if(length(summand) != nrow(multiplier)){
        stop("Dimension mimatch: length(summand) must be equal to nrow(multiplier)!")
     }  
  }
  if(length(summand) > object@dimension){
    stop("Do not extend the dimension of the ghyp distribution, i.e. ncol(multiplier) ",
         "and length(summand) must be <= dimension!")
  }
  if(any(diag(multiplier)==0)){
    stop("Diagonal elements of multiplier must not be '0'!")
  }  
  sigma <- multiplier %*% object@sigma %*% t(multiplier)
  if(ncol(sigma)==1){
    sigma <- as.vector(sigma)
    if(object@dimension==1){
      sigma <- sigma * object@sigma
    }
  }
  if(length(summand)==1){
    return(ghyp(lambda = object@lambda,chi=object@chi,psi=object@psi,
                mu =  as.vector(multiplier %*% object@mu + summand),
                sigma=sqrt(sigma),
                gamma=as.vector(multiplier %*% object@gamma)))
  }else{
    return(ghyp(lambda = object@lambda,chi=object@chi,psi=object@psi,
                mu =  as.vector(multiplier %*% object@mu + summand),
                sigma=sigma,
                gamma=as.vector(multiplier %*% object@gamma)))
  }
}
