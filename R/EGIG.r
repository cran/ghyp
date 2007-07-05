"Egig" <- function(lambda,chi,psi,func=c("x","logx","1/x","var"), check.pars = T){
  if(check.pars)
    check.gig.pars(lambda,chi,psi)
  
  chi.eps <- .Machine$double.eps

  func <- match.arg(func)
  params <- suppressWarnings(cbind(as.vector(lambda),
                                   as.vector(chi),
                                   as.vector(psi)))
  lambda <- params[,1]
  chi <- params[,2]
  psi <- params[,3] 

  if(func=="x"){
    if(all(psi==0)){
      # Inv Gamma -> student t
      beta <- 0.5 * chi
      alpha <- -lambda
      return(beta/(alpha-1))
    }else if(all(chi==0)){
      # Gamma -> VG      
      beta <- 0.5 * psi
      alpha <- lambda
      return(alpha/beta)
    }else{
      # GIG -> ghyp, hyp, NIG
      chi[abs(chi)< chi.eps] <- sign(chi[abs(chi)< chi.eps])* chi.eps
      chi[chi==0] <- chi.eps

      alpha.bar <- sqrt(chi*psi)
      term1 <- 0.5*log(chi/psi)
      term2 <- besselM3(lambda+1,alpha.bar,logvalue=T)
      term3 <- besselM3(lambda,alpha.bar,logvalue=T)
      return(exp(term1+term2-term3))
    }
  }else if(func=="logx"){
    if(all(psi==0)){
      # Inv Gamma -> student t
      beta <- 0.5 * chi
      alpha <- -lambda
      return(log(beta)-digamma(alpha))
    }else if(all(chi==0)){
      # Gamma -> VG      
      beta <- 0.5 * psi
      alpha <- lambda
      return(digamma(alpha)-log(beta))
    }else{
      # GIG -> ghyp, hyp, NIG

      chi[abs(chi)< chi.eps] <- sign(chi[abs(chi)< chi.eps])* chi.eps
      chi[chi==0] <- chi.eps
      
      alpha.bar <- sqrt(chi*psi)
      besselKnu <- function(nu, xx, expon.scaled=F) {besselK(xx, nu, expon.scaled)}
      Kderiv <- numDeriv::grad(besselKnu, lambda, 
                        method.args=list(eps=1e-8,show.details=F), xx=alpha.bar)
      return(0.5*log(chi/psi) + Kderiv/besselK(alpha.bar, lambda))
    }    
  }else if(func=="1/x"){
    if(all(psi==0)){
      # Inv Gamma -> student t
      beta <- 0.5 * chi
      alpha <- -lambda
      return(alpha/beta) 
    }else if(all(chi==0)){
      # Gamma -> VG      
      warning("Case 'chi==0' and 'func = 1/x' is not implemented")
      return(NA)
    }else{
      # GIG -> ghyp, hyp, NIG
      chi[abs(chi)< chi.eps] <- sign(chi[abs(chi)< chi.eps])* chi.eps
      chi[chi==0] <- chi.eps
      
      alpha.bar <- sqrt(chi*psi)
      term1 <- -0.5*log(chi/psi)
      term2 <- besselM3(lambda-1,alpha.bar,logvalue=T)
      term3 <- besselM3(lambda,alpha.bar,logvalue=T)
      return(exp(term1+term2-term3))
    }
  }else if(func=="var"){
    if(all(psi==0)){
      # Inv Gamma -> student t
      beta <- 0.5 * chi
      alpha <- -lambda
      return(beta^2/((alpha-1)^2*(alpha-2)))
    }else if(all(chi==0)){
      # Gamma -> VG      
      beta <- 0.5 * psi
      alpha <- lambda
      return(alpha/(beta^2))
    }else{
      # GIG -> ghyp, hyp, NIG
      chi[abs(chi)< chi.eps] <- sign(chi[abs(chi)< chi.eps])* chi.eps
      chi[chi==0] <- chi.eps
      
      alpha.bar <- sqrt(chi*psi)
      term1 <- 0.5*log(chi/psi)
      term2 <- besselM3(lambda+1,alpha.bar,logvalue=T)
      term3 <- besselM3(lambda,alpha.bar,logvalue=T)
      var.term1 <- log(chi/psi)
      var.term2 <- besselM3(lambda+2,alpha.bar,logvalue=T)
      var.term3 <- besselM3(lambda,alpha.bar,logvalue=T)        
      return(exp(var.term1+var.term2-var.term3)-exp(term1+term2-term3)^2)
    }
  
  }
}
