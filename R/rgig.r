"rgig" <- function(n = 10, lambda = 1, chi = 1, psi = 1, envplot = FALSE,
                   messages = FALSE)
{
  ## This source code is copied from the S-Plus library QRMlib
  ## from Alexander J. McNeil (2005) designed to accompany the book 
  ## Quantitative Risk Management, Concepts, Techniques and Tools.
  ## see http://www.math.ethz.ch/~mcneil/book/QRMlib.html
  if((chi < 0) | (psi < 0))
    stop("Invalid parameters for GIG")
  if((chi == 0) & (lambda <= 0))
    stop("Invalid parameters for GIG")
  if((psi == 0) & (lambda >= 0))
    stop("Invalid parameters for GIG")
  if((chi == 0) & (lambda > 0))
    return(rgamma(n, shape = lambda, rate = (psi/2)))
  if((psi == 0) & (lambda < 0))
    return(1/rgamma(n, shape = ( - lambda), rate = (chi/2)))
  message <- NULL
  if(abs(lambda) < 1)
    message <- "Not necessarily efficient rejection method\n"
  neglambda <- F
  if(lambda < 0){
    neglambda = T
    lambda <- abs(lambda)
    tmp <- c(chi, psi)
    chi <- tmp[2]
    psi <- tmp[1]
  }
  #----------------------------------------------------------
  efunc <- function(x, lambda, chi, psi)
  {
    (x^(lambda - 1)) * exp( - (chi/x + psi * x)/2)
  }
  #----------------------------------------------------------  
  calcmode <- function(lambda, chi, psi)
  {
    if(psi > 0)
      return(((lambda - 1) + sqrt(chi * psi + (1 - lambda)^
        2))/psi)
    else if(psi == 0)
      return(chi/(2 - 2 * lambda))
    else stop("Problem in mode function")
  }
  #----------------------------------------------------------
  themode <- calcmode(lambda, chi, psi)
  assign("lambda", lambda)
  assign("chi", chi)
  assign("psi", psi)
  assign("themode", themode)
  assign("calcmode", calcmode)
  assign("efunc", efunc)
  if(lambda < 1){
    theta <- 0.01
    objective <- function(theta)
    {
      if(theta <= 0)
        out <- NA
      else {
        Delta1 <- (exp(themode * theta) - 1)/theta
        Delta2 <- (2 * exp(( - themode * psi)/2))/
          psi
        xL <- calcmode(lambda, chi, psi + 2 * theta)
        xH <- chi/(2 - 2 * lambda)
        S1 <- efunc(xL, lambda, chi, psi + 2 * theta)
        S2 <- efunc(xH, lambda, chi, 0)
        out <- Delta1 * S1 + Delta2 * S2
      }
      return(out)
    }
    out <- optimize(objective, interval=c(1e-5,100),tol=1e-8)
  }else{
    theta <- c(0.01, psi/4)
    objective <- function(theta)
    {
      if((theta[1] <= 0) | (theta[2] <= 0))
        out <- NA
      else if((psi - 2 * theta[2]) < 0)
        out <- NA
      else {
        Delta1 <- (exp(themode * theta[1]) - 1)/theta[1]
        Delta2 <- exp( - themode * theta[2])/theta[2]
        xL <- calcmode(lambda, chi, psi + 2 * theta[1])
        xH <- calcmode(lambda, chi, psi - 2 * theta[2])
        S1 <- efunc(xL, lambda, chi, psi + 2 * theta[1])
        S2 <- efunc(xH, lambda, chi, psi - 2 * theta[2])
        out <- Delta1 * S1 + Delta2 * S2
      }
      out
    }
    out <- optim(theta, objective, control=list(maxit = 10000))
    if(!(out$convergence==0))
    message <- paste(message,
      "Problems finding optimal s and p (use option envplot for reassurance)",
      "\n")
  }
  if(lambda < 1) {
    spar <- out$minimum
    ppar <- psi/2
  }
  else {
    spar <- out$par[1]
    ppar <- out$par[2]
  }
  xL <- calcmode(lambda, chi, psi + 2 * spar)
  xH <- calcmode(lambda, chi, psi - 2 * ppar)
  S1 <- efunc(xL, lambda, chi, psi + 2 * spar)
  S2 <- efunc(xH, lambda, chi, psi - 2 * ppar)
  Delta1 <- (exp(themode * spar) - 1)/spar
  Delta2 <- exp( - themode * ppar)/ppar
  k <- 1/((Delta1/S2) + (Delta2/S1))
  k1 <- k/S2
  k2 <- k/S1
  rpar <- k1 * Delta1
  if(envplot) {
    xdat <- seq(from = 0.01, to = themode * 20, length = 1000)
    envelope2 <- (xdat <= themode) * exp(spar * xdat) * S1 + (
      xdat > themode) * exp( - ppar * xdat) * S2
    envelope <- (xdat <= themode) * exp(spar * xdat) * k1 + (xdat >
      themode) * exp( - ppar * xdat) * k2
    ydat <- efunc(xdat, lambda, chi, psi)
    yr <- range(ydat, envelope, envelope2)
    plot(xdat, ydat, ylim = yr, type = "l")
    abline(v = themode)
    lines(xdat, envelope, col = 2)
    lines(xdat, envelope2, lty = 2, col = 2)
  }
  xsim = numeric()
  iter=0
  
  
  #############################################################################
  rgigsim = function(n, rpar, spar, ppar, k1, k2, lambda, chi, psi, s1, s2)
  {
    ef = function (x, lambda, chi, psi)
      { x^(lambda-1) * exp(-0.5*(psi*x + chi/x)) }
  
    U = runif(n)
    idx = (U<=rpar)
  
    nLow = sum(idx)
    x = log( 1 + spar*U[idx]/k1 ) / spar
    level = log( ef(x, lambda, chi, psi+2*spar ) / s1 )
    xsim = x[log( runif(nLow) )<=level]
  
    x = -log( ppar*(1-U[!idx])/k2 ) / ppar
    level = log( ef(x, lambda, chi, psi-2*ppar ) / s2 )
    xsim = c(xsim, x[ log( runif(n-nLow) ) <= level ] )
  
    xsim
  }
  #############################################################################   
  
  while (length(xsim)<n) {
    m = n-length(xsim)
    iter = iter+m
    xx = rgigsim(n = m, rpar = rpar, spar = spar, ppar = ppar, k1 = k1, k2 = k2,
      lambda = lambda, chi = chi, psi = psi, s1 = S1, s2 = S2)
    xsim = c(xsim,xx)
  }
  efficiency <- length(xsim)/iter
  message <- paste(message, "Efficiency", round(efficiency * 100, 1),"\n")
  if(messages)
    cat(message)
  if(neglambda){
    return(1/xsim)
  }else{
    return(xsim)
  }
}

