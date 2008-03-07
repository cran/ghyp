"check.gig.pars" <- function(lambda, chi, psi){
    ##<---------------  vectorized check  ---------------->
    "internal.check.gig.pars" <- function(x){
      lambda <- x[1]
      chi <- x[2]
      psi <- x[3]
      if(lambda < 0 & (chi <= 0 | psi < 0)){
        stop("If lambda < 0: chi must be > 0 and psi must be >= 0! \n",
             "lambda = ", lambda, ";   chi = ", chi, ";   psi = ", psi, "\n")
      }
      if(lambda == 0 & (chi <= 0 | psi <= 0)){
        stop("If lambda == 0: chi must be > 0 and psi must be > 0! \n",
             "lambda = ", lambda, ";   chi = ", chi, ";   psi = ", psi, "\n")
      }
      if(lambda > 0 & (chi < 0 | psi <= 0)){
        stop("If lambda > 0: chi must be >= 0 and psi must be > 0! \n",
             "lambda = ", lambda, ";   chi = ", chi, ";   psi = ", psi, "\n")
      }
    }

  params <- suppressWarnings(cbind(lambda, chi, psi))
  apply(params, 1, internal.check.gig.pars)
}
