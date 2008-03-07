"dgig" <- function(x, lambda = 1, chi = 1, psi = 1, logvalue = FALSE){
 check.gig.pars(lambda, chi, psi)
 
  x.raw  <- x

  x.raw[x.raw < 0] <- NA
  x.raw[x.raw == 0] <- 0

  x <- x.raw[is.finite(x.raw) & x.raw > 0]
  if(length(x) == 0){
    return(x.raw)
  } 

 if(psi == 0){
   # Inverse Gamma -> student t case
   beta <- 0.5 * chi
   alpha <- -lambda
   gig.density <- beta^alpha / gamma(alpha) * x^(-alpha - 1) * exp(-beta / x)
 }else if(chi == 0){
   # Gamma -> VG case
   beta <- 0.5 * psi
   alpha <- lambda
   gig.density <- beta^alpha / gamma(alpha) * x^(alpha - 1) * exp(-beta * x)
   
 }else{
   gig.density <- sqrt(psi / chi)^lambda / (2 * besselK(sqrt(chi * psi), lambda)) *
                  x^(lambda - 1) * exp(-0.5 * (chi / x + psi * x))
 }                                   
 
## print(cbind(gig.density, x))
 x.raw[is.finite(x.raw) & x.raw > 0] <- gig.density
 if(logvalue){
   return(log(x.raw))
 }else{
   return(x.raw) 
 }

}

