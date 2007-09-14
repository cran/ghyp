"fit.ghypmv" <- function(data, lambda = 1, alpha.bar = 1,
                         mu = NULL, sigma = NULL, gamma = NULL,
                         opt.pars = c(lambda = T, alpha.bar = T, mu = T, sigma = T, gamma = !symmetric),
                         symmetric = F, standardize = F, nit = 2000, reltol = 1e-8, abstol = reltol * 10, 
                         na.rm = F, silent = FALSE, save.data = T,...)
{
  call <- match.call(expand.dots = TRUE) 

  data <- check.data(data = data, case = "mv", na.rm = na.rm)
  n <- nrow(data)
  d <- ncol(data)

  tmp.abar2chipsi <- abar2chipsi(alpha.bar, lambda)
  chi <- tmp.abar2chipsi$chi
  psi <- tmp.abar2chipsi$psi
  #<----- check parameters of the mixing distribution for consistency --------->
  check.gig.pars(lambda, chi, psi)
  #<----------------- check opt.pars  for consistency ------------------------->
  opt.pars <- check.opt.pars(opt.pars, symmetric)
  if(is.null(mu)){
    mu <- colMeans(data)
  }
  if(is.null(gamma) | symmetric){
    gamma <- rep(0, d)
  }
  if(symmetric){
    opt.pars["gamma"] <- FALSE
  }
  if(is.null(sigma)){
    sigma <- var(data)
  }

  #<--------- check normal and skewness parameters  for consistency ----------->
  check.norm.pars(mu, sigma, gamma, d)

  if(standardize){
    # data will be standardized and initial values will be adapted
    tmp.mean <- colMeans(data)
    tmp.var <- var(data)

    sigma.chol <- t(chol(solve(tmp.var)))
    
    data.backup <- data    
    data <- apply(data.backup, MARGIN = 2, FUN = function(x){x - mean(x)})
    data <-  data %*% sigma.chol
    
    sigma <- t(sigma.chol) %*% sigma %*% sigma.chol
    gamma <- as.vector(sigma.chol %*% gamma)
    mu <- as.vector(sigma.chol %*% (mu - tmp.mean))
  }  

  #<------- loglikelihood function of the inverse gamma distribution ---------->
  t.optfunc <- function(thepars, delta.sum, xi.sum, n.rows){
      nu <- -2 * t.transform(thepars)
      term1 <- -n.rows * nu * log(nu/2 - 1)/2
      term2 <- (nu/2 + 1) * xi.sum + (nu/2 - 1) * delta.sum
      term3 <- n.rows * lgamma(nu/2)
      out <- term1 + term2 + term3
      return(out)
  }

  #<------------ loglikelihood function of the gamma distribution ------------->
  vg.optfunc <- function(thepars, xi.sum, eta.sum, n.rows){
      thepars <- exp(thepars)
      term1 <- n.rows * (thepars * log(thepars) - lgamma(thepars))
      term2 <- (thepars - 1) * xi.sum - thepars * eta.sum
      out <- -(term1 + term2)
      return(out)
  }
  
  #<- loglikelihood function of the generalized inverse gaussian distribution ->
  gig.optfunc <- function(thepars, mix.pars.fixed, delta.sum, eta.sum, xi.sum, n.rows)
  {
    out <- NA
    tmp.pars <- c(thepars, mix.pars.fixed)
    lambda <- tmp.pars["lambda"]
    alpha.bar <- exp(tmp.pars["alpha.bar"])
    tmp.abar2chipsi <- abar2chipsi(alpha.bar, lambda)
    chi <- tmp.abar2chipsi$chi
    psi <- tmp.abar2chipsi$psi
    if (lambda < 0 & psi == 0){                             # t
     out <-  t.optfunc(lambda, delta.sum, xi.sum, n.rows)
    }else if(lambda > 0 & chi == 0) {                       # VG
        out <-  vg.optfunc(lambda, xi.sum, eta.sum, n.rows)
     }else{                                                 # ghyp, hyp, NIG
        term1 <- (lambda - 1) * xi.sum
        term2 <- -chi * delta.sum/2
        term3 <- -psi * eta.sum/2
        term4 <- -n.rows * lambda * log(chi)/2 + n.rows * lambda * log(psi)/2 -
                  n.rows * besselM3(lambda, sqrt(chi * psi), logvalue=T)
        out <- -(term1 + term2 + term3 + term4)
    }
    return(out)
  }
             
  #<------------------------- Initialize fitting loop ------------------------->
  i <- 0
  rel.closeness <- 100
  abs.closeness <- 100
  tmp.fit <- list(convergence = 0, message = NULL)
  
  ll <-  sum(internal.dghypmv(data, lambda = lambda, chi = chi, psi = psi,
                              mu = mu, sigma = sigma, gamma = gamma, logvalue=T))  
  #<------------------------- Start interations ------------------------------->
  while ((abs.closeness > abstol) & (rel.closeness > reltol) & (i < nit)){
    i <- i+1
    #<------------------------ E-Step: EM update ------------------------------>
    # The parameters mu, sigma and gamma become updated
    inv.sigma <- solve(sigma)
    Q <- mahalanobis(data, mu, inv.sigma, inverted = TRUE)
    Offset <- t(gamma) %*% inv.sigma %*% gamma
    delta <- Egig(lambda-d/2,Q+chi,psi+Offset,func="1/x", check.pars = FALSE)
    delta.bar <- mean(delta)
    eta <- Egig(lambda-d/2,Q+chi,psi+Offset,func="x", check.pars = FALSE)
    eta.bar <- mean(eta)
    delta.matrix <- matrix(delta,nrow=n,ncol=d,byrow=F)
    if (opt.pars["gamma"]) {
      Xbar.matrix <- matrix(apply(data,2,mean),nrow=n,ncol=d,byrow=T)
      Xbar.matrix <- Xbar.matrix-data
      gamma <- apply(delta.matrix*Xbar.matrix,2,sum)/(n*delta.bar*eta.bar-n)
    }
    if (opt.pars["mu"]) {
      mu <- (apply(delta.matrix*data,2,sum)/n - gamma)/delta.bar
    }
    mu.matrix <- matrix(mu,nrow=n,ncol=d,byrow=T)
    standardised <- data-mu.matrix
    tmp <- delta.matrix*standardised
    if (opt.pars["sigma"]) {
      sigma <- (t(tmp) %*% standardised)/n - outer(gamma,gamma)*eta.bar
    }
    #<------------------------ M-Step: EM update ------------------------------>
    # Maximise the conditional likelihood function and estimate lambda, chi, psi
    inv.sigma <- solve(sigma)
    Q <- mahalanobis(data, mu, inv.sigma, inverted = TRUE)
    Offset <- t(gamma) %*% inv.sigma %*% gamma
    xi.sum <- sum(Egig(lambda-d/2,Q+chi,psi+Offset,func="logx", check.pars = FALSE))
    if(alpha.bar==0 & lambda > 0 & !opt.pars["alpha.bar"] & opt.pars["lambda"]){ # VG case
      eta.sum <- sum(Egig(lambda-d/2,Q+chi,psi+Offset,func="x", check.pars = FALSE))
      
      tmp.fit <- suppressWarnings(optim(log(lambda), vg.optfunc, eta.sum=eta.sum, 
                                        xi.sum=xi.sum, n.rows=n,...))
      lambda <- exp(tmp.fit$par)                   
    }else if(alpha.bar==0 & lambda < 0 & !opt.pars["alpha.bar"] & opt.pars["lambda"]){ # Student-t case
      delta.sum <- sum(Egig(lambda-d/2,Q+chi,psi+Offset,func="1/x", check.pars = FALSE))
      tmp.fit <- suppressWarnings(optim(inv.t.transform(lambda), t.optfunc, delta.sum=delta.sum, 
                                        xi.sum=xi.sum, n.rows=n,...))
      lambda <- t.transform(tmp.fit$par)    
    }else if(opt.pars["lambda"] | opt.pars["alpha.bar"]){  ## ghyp, hyp, NIG case
      delta.sum <- sum(Egig(lambda-d/2,Q+chi,psi+Offset,func="1/x", check.pars = FALSE))
      eta.sum <- sum(Egig(lambda-d/2,Q+chi,psi+Offset,func="x", check.pars = FALSE))
      
      mix.pars <- c(lambda=unname(lambda),alpha.bar=log(unname(alpha.bar)))
      opt.pars.mix <- opt.pars[c("lambda","alpha.bar")]
      thepars <- mix.pars[opt.pars.mix]
      
      tmp.fit <- suppressWarnings(optim(thepars, gig.optfunc, 
                                        mix.pars.fixed=mix.pars[!opt.pars.mix], 
                                        delta.sum=delta.sum,eta.sum=eta.sum, 
                                        xi.sum=xi.sum, n.rows=n,...))
      
      lambda <- c(tmp.fit$par,mix.pars[!opt.pars.mix])["lambda"]
      alpha.bar <- exp(c(tmp.fit$par,mix.pars[!opt.pars.mix])["alpha.bar"])
    }
    tmp.abar2chipsi <- abar2chipsi(alpha.bar,lambda)
    chi <- tmp.abar2chipsi$chi
    psi <- tmp.abar2chipsi$psi
    #<------------------------ Test for convergence --------------------------->
    ll.old <- ll
    ll <-  sum(internal.dghypmv(data,lambda=lambda,chi=chi,psi=psi,
               mu=mu, sigma=sigma,gamma= gamma, logvalue=T ))
    abs.closeness <- abs(ll-ll.old)
    rel.closeness <- abs((ll-ll.old)/ll.old)
    if(!is.finite(abs.closeness) | !is.finite(rel.closeness)){
      warning("fit.ghypmv: Loglikelihood is not finite! Iteration stoped!\n",
      "Loglikelihood :",ll)
      break
    }
    #<--------------------------- Print result -------------------------------->
    if(!silent){   
      message <- paste("iter: ",i,"; rel.closeness: ",sprintf("% .6E",rel.closeness),
        "; log-likelihood: ",sprintf("% .6E",ll),"; alpha.bar: ",sprintf("% .6E",alpha.bar),
        "; lambda: ",sprintf("% .6E",lambda),sep="")
      print(message)
    }
  }      
  # END OF WHILE LOOP

  conv <- tmp.fit$convergence
  conv.type <- tmp.fit$message
  if(is.null(tmp.fit$message)){
    conv.type <- ""
  }else{
    conv.type <- paste("Message from 'optim':",tmp.fit$message)
  }

  converged <- F
  if(i < nit & is.finite(rel.closeness) & is.finite(abs.closeness)){
    converged <- T
  }

  if(standardize){
    data <- data.backup 
    inv.sigma.chol <- solve(sigma.chol)
    mu <- as.vector(inv.sigma.chol %*% mu + tmp.mean)
    sigma <- t(inv.sigma.chol) %*% sigma %*% inv.sigma.chol
    gamma <- as.vector(inv.sigma.chol %*% gamma)
    abar.chi.psi <- abar2chipsi(alpha.bar = alpha.bar, lambda = lambda)
    ll <- sum(internal.dghypmv(x = data, lambda = lambda, chi = abar.chi.psi$chi,
                               psi = abar.chi.psi$psi, mu = mu, sigma = sigma, 
                               gamma = gamma, logvalue = TRUE))
  }

  if(!save.data){
    data <- NULL
  }

  nbr.fitted.params <- unname(sum(opt.pars[c("alpha.bar","lambda")]) + 
                              d * sum(opt.pars[c("mu","gamma")]) +
                              d/2 * (d + 1) * opt.pars[c("sigma")])
  aic <- -2 * ll + 2 * nbr.fitted.params 
  
  ghyp.object <- ghyp(lambda = lambda, alpha.bar = alpha.bar,
                      mu = mu, sigma = sigma, gamma = gamma, data = data)
  
  ghyp.object@parametrization = "lambda.alpha.bar"
  ghyp.object@call = call 
  
  return(fit.ghyp(ghyp.object, llh = ll, n.iter = i, converged = converged,
                  error.code = conv, error.message = conv.type,
                  fitted.params = opt.pars, aic = aic))
}


