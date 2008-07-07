"fit.ghypuv" <- function(data, lambda = 1, alpha.bar = 0.5, mu = median(data), 
                         sigma = mad(data), gamma = 0, 
                         opt.pars = c(lambda = T, alpha.bar = T, mu = T, sigma = T, gamma = !symmetric), 
                         symmetric = F, standardize = F, save.data = T, na.rm = T, silent = FALSE, ...)
{
  call <- match.call(expand.dots = TRUE) 
  
  #<------------ Check input ---------------->
  opt.pars <- check.opt.pars(opt.pars, symmetric)
  data <- check.data(data = data, case = "uv", na.rm = na.rm)
  if(standardize){
    # data will be standardized and initial values will be adapted
    tmp.mean <- mean(data)
    tmp.sd <- sd(data)
    data.backup <- data
    data <- (data - tmp.mean)/tmp.sd
    mu <- (mu - tmp.mean)/tmp.sd
    sigma <- sigma/tmp.sd
    gamma <- gamma/tmp.sd
  }

  check.norm.pars(mu, sigma, gamma, 1)

  tmp.abar2chipsi <- abar2chipsi(alpha.bar, lambda)
  chi <- tmp.abar2chipsi$chi
  psi <- tmp.abar2chipsi$psi
  #<----- check parameters of the mixing distribution for consistency --------->
  check.gig.pars(lambda, chi, psi)
  
  var.names <- c("lambda", "alpha.bar", "mu", "sigma", "gamma")

  vars <- unname(c(lambda, alpha.bar, mu, sigma, gamma))
  names(vars) <- var.names
  
  if(alpha.bar == 0 & !opt.pars["alpha.bar"]){  
    if(lambda > 0){                        # VG case
       transform <- c("exp", "exp")
       inv.transform <- c("log", "log")
    }else{                                 # Student-t case
       transform <- c("t.transform", "exp")
       inv.transform <- c("inv.t.transform", "log")
    }
    names(transform) <- names(inv.transform) <- c("lambda", "sigma")
  }else{                                   # ghyp, NIG, hyp case
    transform <- c("exp", "exp")
    inv.transform <- c("log", "log")
    names(transform) <- names(inv.transform) <- c("alpha.bar", "sigma")
  }
  
  

  ## Inverse transformation of the initial parameter values
  for(nam in intersect(names(opt.pars[opt.pars]), names(transform))) {
    vars[nam] <- do.call(inv.transform[nam], list(vars[nam]))
  }
  tmp.fit <- mle.default(data = data, pdf = "internal.dghyp",
                         vars = vars, opt.pars = opt.pars,
                         transform = transform, se = T,
                         silent = silent, ...)

  if(tmp.fit$convergence != 0){
    converged <- FALSE
  }else{
    converged <- TRUE
  }

  if(is.null(tmp.fit$message)){
    conv.type <- ""
  }else{
    conv.type <- paste("Message from 'optim':", tmp.fit$message)
  }

  if(standardize){
    data <- data.backup 
    tmp.fit$par.ests["mu"] <- tmp.fit$par.ests["mu"] * tmp.sd + tmp.mean
    tmp.fit$par.ests["sigma"] <- tmp.fit$par.ests["sigma"] * tmp.sd
    tmp.fit$par.ests["gamma"] <- tmp.fit$par.ests["gamma"] * tmp.sd
    tmp.llh <- try(sum(internal.dghyp(x = data, lambda = tmp.fit$par.ests["lambda"],
                                             alpha.bar = tmp.fit$par.ests["alpha.bar"],
                                             mu = tmp.fit$par.ests["mu"],
                                             sigma = tmp.fit$par.ests["sigma"],
                                             gamma = tmp.fit$par.ests["gamma"],
                                             logvalue = TRUE)))
    if(class(tmp.llh) == "try-error"){
      warning("Error occured during renormalization! Log-likelihood set to zero!\n")
      tmp.fit$ll.max <- as.numeric(NA)
    }else{
      tmp.fit$ll.max <- tmp.llh
    }
    tmp.fit$parameter.variance <- matrix(NA)
  }

  nbr.fitted.params <- unname(sum(opt.pars))
  aic <- -2 * tmp.fit$ll.max + 2 * nbr.fitted.params 
  
  if(!save.data){
    data <- NULL
  }

  ghyp.object <- ghyp(lambda = tmp.fit$par.ests["lambda"],
                      alpha.bar = tmp.fit$par.ests["alpha.bar"],
                      mu = tmp.fit$par.ests["mu"],
                      sigma = tmp.fit$par.ests["sigma"],
                      gamma = tmp.fit$par.ests["gamma"],
                      data = data)

  ghyp.object@call <- call 
  return(fit.ghyp(ghyp.object, llh = tmp.fit$ll.max, n.iter = tmp.fit$n.iter,
                  converged = converged,
                  error.code = tmp.fit$convergence, error.message = conv.type,
                  parameter.variance = tmp.fit$parameter.variance,
                  fitted.params = opt.pars, aic = aic, trace.pars = as.list(as.data.frame(tmp.fit$trace.pars))))
}
