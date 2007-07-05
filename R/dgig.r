"dgig" <- function(x,lambda=1,chi=1,psi=1){
 check.gig.pars(lambda,chi,psi)
 if(psi==0){
   # Inverse Gamma -> student t case
   beta <- 0.5 * chi
   alpha <- -lambda
   return(beta^alpha/gamma(alpha)*x^(-alpha-1)*exp(-beta/x))
 }else if(chi==0){
   # Gamma -> VG case
   beta <- 0.5 * psi
   alpha <- lambda
   return(beta^alpha/gamma(alpha)*x^(alpha-1)*exp(-beta*x))
   
 }else{
    return(sqrt(psi/chi)^lambda / (2 * besselK(sqrt(chi*psi),lambda))*
            x^(lambda-1)*exp(-0.5*(chi/x+psi*x)))
 }                                   

}

