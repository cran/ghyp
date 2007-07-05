"gh.model" <- function(lambda,chi,psi,gamma){
  d <- length(gamma)
  if(lambda==(d+1)/2 & chi > 0 & psi > 0){
    gh.case <- "Hyperbolic"
  }else if(lambda==-0.5 & chi > 0 & psi > 0){
    gh.case <- "Normal Inverse Gaussian"
  }else if(lambda > 0 & chi == 0){
    gh.case <- "Variance Gamma"
  }else if(lambda < 0 & psi == 0){
    gh.case <- "Student-t"
  }else{
    gh.case <- "Generalized Hyperbolic"
  }
  if(sum(abs(gamma))==0){
    gh.case <- paste("Symmetric",gh.case)
  }else{
    gh.case <- paste("Asymmetric",gh.case)
  }
  return(gh.case)
}
