#<=================== chi.psi and alpha.bar - parametrization =================>
#<--------------------------------   HYP   ------------------------------------>
"hyp" <- function(chi = 0.5, psi = 2, mu = 0, sigma = diag(rep(1, length(mu))),
                   gamma = rep(0, length(mu)), alpha.bar = NULL, data = NULL)
{
  call <- match.call()

  if(is.null(alpha.bar)){
    parametrization <- "chi.psi"
  }else{
    parametrization <- "alpha.bar"  
  }
  ghyp.object <- ghyp(lambda = (length(mu)+1)/2, chi = chi, psi = psi, mu = mu, 
                      sigma = sigma, gamma = gamma, alpha.bar = alpha.bar, data = data)
  ghyp.object@call <- call
  ghyp.object@parametrization <- parametrization
  return(ghyp.object)
}
#<--------------------------------   NIG   ------------------------------------>
"NIG" <- function(chi = 2, psi = 2, mu = 0, sigma = diag(rep(1, length(mu))),
                   gamma = rep(0, length(mu)), alpha.bar = NULL, data = NULL)
{
  call <- match.call()

  if(is.null(alpha.bar)){
    parametrization <- "chi.psi"
  }else{
    parametrization <- "alpha.bar"  
  }
  ghyp.object <- ghyp(lambda = -0.5, chi = chi, psi = psi, mu = mu, sigma = sigma, 
                      gamma = gamma, alpha.bar = alpha.bar, data = data)
  ghyp.object@call <- call
  ghyp.object@parametrization <- parametrization
  return(ghyp.object)
}
#<--------------------------------   Student-t   ------------------------------------>
"student.t" <- function(nu = 3.5, mu = 0, sigma = diag(rep(1, length(mu))),
                   gamma = rep(0, length(mu)), data = NULL)
{
  call <- match.call()

  parametrization <- "alpha.bar"  
  ghyp.object <-  ghyp(lambda = -nu/2, psi = 0, mu = mu, sigma = sigma, 
                       gamma = gamma, alpha.bar = 0, data = data)
  ghyp.object@call <- call
  ghyp.object@parametrization <- parametrization
  return(ghyp.object)
}
#<--------------------------------   VG   ------------------------------------>
"VG" <- function(lambda = 1, psi = 2*lambda, mu = 0, sigma = diag(rep(1, length(mu))),
                   gamma = rep(0, length(mu)), data = NULL)
{
  call <- match.call()

  if(psi == 2 * lambda){
    parametrization <- "alpha.bar"  
  }else{
    parametrization <- "chi.psi"
  }
  ghyp.object <- ghyp(lambda = lambda, chi = 0, psi = psi, mu = mu, sigma = sigma, 
                      gamma = gamma, alpha.bar = NULL, data = data)
  ghyp.object@call <- call
  ghyp.object@parametrization <- parametrization
  return(ghyp.object)
}

#<====================== alpha.delta - parametrization =========================>
"ghyp.ad" <- function(lambda = 0.5, alpha = 1.5, delta = 1, beta = rep(0, length(mu)), 
                      mu = 0, Delta = diag(rep(1, length(mu))), data = NULL)
{
  call <- match.call()

  # Test validity of parameters
  if(delta < 0){
    stop("Parameter 'delta' must be >= 0!")
  }
  if(delta == 0 && lambda <= 0){
    stop("Parameter 'lambda' must be > 0 when 'delta' = 0!")
  }
  if(length(lambda) != 1 || length(alpha) != 1 || length(delta) != 1){
    stop("Parameters 'lambda', 'alpha' and 'delta' must be of length 1!")
  }
  if(length(beta) != length(mu)){
    stop("Length of 'beta' must be equal to length of 'mu'!")
  }
  if(length(mu) != dim(as.matrix(Delta))[1] || length(mu) != dim(as.matrix(Delta))[2]){
    stop("dim(Delta) must be length(mu) x length(mu)!")
  }
  chi <- delta^2
  if(length(mu) == 1){ #Univariate case
    psi <- alpha^2 - beta^2
    if(!missing(Delta)){
      warning("Parameter 'Delta' ignored in the univariate case!")
    }
    if(alpha^2 - beta^2 < 0){
      stop("alpha^2 - beta^2 must be >= 0!")
    }
    if(isTRUE(all.equal(psi, 0, tol = 1e-8)) & (lambda >= 0)){
      stop("Student-t Distribution: lambda must be < 0 when alpha^2 - beta' Delta beta = 0!")
    }
    gamma <- beta
    ghyp.object <- ghyp(chi = chi, psi = psi, lambda = lambda, 
                        mu = mu, sigma = 1, gamma = gamma, data = data)
    ghyp.object@call <- call
    ghyp.object@parametrization <- "alpha.delta"
    return(ghyp.object)
  }else{ # Multivariate case
    if(!isTRUE(all.equal(det(Delta), 1, tol = .Machine$double.eps^0.5))){
      stop("Determinant of 'Delta' must be 1!")
    }   
    psi <- as.numeric(alpha^2 - beta %*% Delta %*% beta)
    if(abs(psi) < .Machine$double.eps^0.5){
      psi <- 0
    }
    if(psi < 0){
      stop("alpha^2 - beta' * Delta * beta must be >= 0!")
    }
    if(isTRUE(all.equal(psi, 0, tol = .Machine$double.eps^0.5)) & (lambda >= 0)){
      stop("Student-t Distribution: lambda must be < 0 when alpha^2 - beta' * Delta * beta = 0!")
    }
    sigma <- Delta
    gamma <- as.numeric(Delta %*% beta)
    ghyp.object <- ghyp(chi = chi, psi = psi, lambda = lambda, 
                        mu = mu, sigma = sigma, gamma = gamma, data = data)    
    ghyp.object@call <- call
    ghyp.object@parametrization <- "alpha.delta"
    return(ghyp.object)                  
   }
}

#<--------------------------------   HYP   ------------------------------------>
"hyp.ad" <- function(alpha = 1.5, delta = 1, beta = rep(0, length(mu)), mu = 0, 
                     Delta = diag(rep(1, length(mu))), data = NULL)
{
  call <- match.call()
  if(length(beta) == 1 && length(mu) == 1){   # Univariate case
    if(!missing(Delta)){
      warning("Parameter 'Delta' ignored in the univariate case!")
    }
    ghyp.object <- ghyp.ad(lambda = (length(mu) + 1)/2, alpha = alpha, delta = delta, 
                           mu = mu, beta = beta, data = data) 
  }else{     # Multivariate case
    ghyp.object <- ghyp.ad(lambda = (length(mu) + 1)/2, alpha = alpha, delta = delta, 
                           mu = mu, Delta = Delta, beta = beta, data = data)   
  }
  ghyp.object@call <- call
  return(ghyp.object) 

}

#<--------------------------------   NIG   ------------------------------------>
"NIG.ad" <- function(alpha = 1.5, delta = 1, beta = rep(0, length(mu)), mu = 0, 
                     Delta = diag(rep(1, length(mu))), data = NULL)
{
  call <- match.call()
  if(length(beta) == 1 && length(mu) == 1){   # Univariate case
    if(!missing(Delta)){
      warning("Parameter 'Delta' ignored in the univariate case!")
    }
    ghyp.object <- ghyp.ad(lambda = -0.5, alpha = alpha, delta = delta, 
                           mu = mu, beta = beta, data = data)  
  }else{     # Multivariate case
    ghyp.object <- ghyp.ad(lambda = -0.5, alpha = alpha, delta = delta, 
                           mu = mu, Delta = Delta, beta = beta, data = data)  
  }
  ghyp.object@call <- call
  return(ghyp.object) 
}

#<--------------------------------   Student-t   ------------------------------------>
"student.t.ad" <- function(lambda = -2, delta = 1, beta = rep(0, length(mu)), mu = 0, 
                           Delta = diag(rep(1, length(mu))), data = NULL)
{
  call <- match.call()
  if(length(beta) == 1 && length(mu) == 1){   # Univariate case
    if(!missing(Delta)){
      warning("Parameter 'Delta' ignored in the univariate case!")
    }
    ghyp.object <- ghyp.ad(lambda = lambda, alpha = sqrt(as.numeric(beta %*% as.matrix(Delta) %*% beta)), 
                           delta = delta, mu = mu, beta = beta, data = data) 
  }else{     # Multivariate case
    ghyp.object <- ghyp.ad(lambda = lambda, alpha = sqrt(as.numeric(beta %*% as.matrix(Delta) %*% beta)), 
                           delta = delta, beta = beta, mu = mu, Delta = Delta, data = data) 
  }
  ghyp.object@call <- call
  return(ghyp.object) 

}

#<--------------------------------   VG   ------------------------------------>
"VG.ad" <- function(lambda = 2, alpha = 1.5, beta = rep(0, length(mu)), mu = 0, 
                    Delta = diag(rep(1, length(mu))), data = NULL)
{
  call <- match.call()
  if(length(beta) == 1 && length(mu) == 1){   # Univariate case
    if(!missing(Delta)){
      warning("Parameter 'Delta' ignored in the univariate case!")
    }
    ghyp.object <- ghyp.ad(lambda = lambda, alpha = alpha, delta = 0, 
                           mu = mu, beta = beta, data = data) 
  }else{     # Multivariate case
    ghyp.object <- ghyp.ad(lambda = lambda, alpha = alpha, delta = 0, 
                           mu = mu, Delta = Delta, beta = beta, data = data) 
  }
  ghyp.object@call <- call
  return(ghyp.object) 

}
#<--------------------------------   NIG   ------------------------------------>
"gauss" <- function(mu = 0, sigma = diag(rep(1, length(mu))), data = NULL)
{
  call <- match.call()
    ghyp.object <- ghyp(chi = Inf, psi = Inf, lambda = as.numeric(NA), 
                        mu = mu, sigma = sigma, gamma = rep(0, length(mu)), data = data)    
    ghyp.object@call <- call
    ghyp.object@parametrization <- "Gaussian"
    return(ghyp.object)   

}
