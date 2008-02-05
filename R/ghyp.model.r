"ghyp.model" <- function(lambda,chi,psi,gamma){
  d <- length(gamma)
  if(lambda == (d + 1)/2 & chi > 0 & psi > 0){
    ghyp.case.long <- "Hyperbolic"
    ghyp.case.short <- "hyp"
  }else if(lambda == -0.5 & chi > 0 & psi > 0){
    ghyp.case.long <- "Normal Inverse Gaussian"
    ghyp.case.short <- "NIG"
  }else if(lambda > 0 & chi == 0){
    ghyp.case.long <- "Variance Gamma"
    ghyp.case.short <- "VG"
  }else if(lambda < 0 & psi == 0){
    ghyp.case.long <- "Student-t"
    ghyp.case.short <- "t"
  }else{
    ghyp.case.long <- "Generalized Hyperbolic"
    ghyp.case.short <- "ghyp"
  }
  if(sum(abs(gamma)) == 0){
    ghyp.case.long.skew <- paste("Symmetric", ghyp.case.long)
    ghyp.case.short.skew <- paste("Symm", ghyp.case.short)
  }else{
    ghyp.case.long.skew <- paste("Asymmetric", ghyp.case.long)
    ghyp.case.short.skew <- paste("Asymm", ghyp.case.short)
  }
  return(unname(c(ghyp.case.long.skew, ghyp.case.long, 
                  ghyp.case.short.skew, ghyp.case.short)))
}
