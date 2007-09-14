"ghyp.model" <- function(lambda,chi,psi,gamma){
  d <- length(gamma)
  if(lambda == (d + 1)/2 & chi > 0 & psi > 0){
    ghyp.case <- "Hyperbolic"
  }else if(lambda == -0.5 & chi > 0 & psi > 0){
    ghyp.case <- "Normal Inverse Gaussian"
  }else if(lambda > 0 & chi == 0){
    ghyp.case <- "Variance Gamma"
  }else if(lambda < 0 & psi == 0){
    ghyp.case <- "Student-t"
  }else{
    ghyp.case <- "Generalized Hyperbolic"
  }
  if(sum(abs(gamma)) == 0){
    ghyp.case <- paste("Symmetric", ghyp.case)
  }else{
    ghyp.case <- paste("Asymmetric", ghyp.case)
  }
  return(ghyp.case)
}
