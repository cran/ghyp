#<--------------------------------   hyp   ------------------------------------>
"fit.hypmv" <- function(data, opt.pars = c(alpha.bar = T, mu = T, sigma = T, gamma = !symmetric), 
                        symmetric = F, ...)
{
  call <- match.call(expand.dots = TRUE) 

  if(!is.null(list(...)$lambda)){
    stop("Do not submit lambda! Lambda is defined as (dimension+1)/2.\n")
  }
  lambda <- (min(dim(data))+1)/2
  
  ghyp.object <- fit.ghypmv(data = data, lambda = lambda, 
                            opt.pars = c(lambda = F, opt.pars), ...)

  ghyp.object@parametrization <- "alpha.bar"
  ghyp.object@call <- call
  return(ghyp.object) 
}
#<--------------------------------   NIG   ------------------------------------>
"fit.NIGmv" <- function(data, opt.pars = c(alpha.bar = T, mu = T, sigma = T, gamma = !symmetric), 
                        symmetric = F, ...)
{
  call <- match.call(expand.dots = TRUE) 

  if(!is.null(list(...)$lambda)){
    stop("Do not submit lambda! Lambda is defined as -0.5.\n")
  }
  ghyp.object <- fit.ghypmv(data = data, lambda = -0.5, 
                            opt.pars = c(lambda = F, opt.pars),...)

  ghyp.object@parametrization <- "alpha.bar"
  ghyp.object@call <- call
  return(ghyp.object) 
}
#<--------------------------------   Student-t   ------------------------------------>
"fit.tmv" <- function(data, nu = 3.5, opt.pars = c(lambda = T, mu = T, sigma = T, gamma = !symmetric), 
                      symmetric = F, ...)
{
  call <- match.call(expand.dots = TRUE) 

  if(nu < 0){
    warning("nu < 0, Variance Gamma distribution is fitted instead!\n")
  }
  if(!is.null(list(...)$alpha.bar)){
    stop("Do not submit alpha.bar! alpha.bar is defined as 0.\n")
  }
  if(!is.null(list(...)$lambda)){
    stop("Do not submit lambda! Student-t distribution works with 'nu'.")
  }
  if("nu" %in% names(opt.pars)){
    names(opt.pars)[which(names(opt.pars)=="nu")] <- "lambda"
  }  
  ghyp.object <- fit.ghypmv(data = data, lambda = -nu/2, alpha.bar = 0, 
                            opt.pars = c(alpha.bar = F, opt.pars), ...)

  ghyp.object@parametrization <- "alpha.bar"
  ghyp.object@call <- call
  return(ghyp.object) 
}
#<--------------------------------   VG   ------------------------------------>
"fit.VGmv" <- function(data, lambda = 1, opt.pars = c(lambda = T, mu = T, sigma = T, gamma = !symmetric), 
                       symmetric = F, ...)
{
  call <- match.call(expand.dots = TRUE) 

  if(lambda < 0){
    warning("lambda < 0, Student-t distribution is fitted instead!\n")
  }
  if(!is.null(list(...)$alpha.bar)){
    stop("Do not submit alpha.bar! alpha.bar is defined as 0.\n")
  }
  ghyp.object <- fit.ghypmv(data = data, lambda = lambda, alpha.bar = 0, 
                            opt.pars = c(alpha.bar = F, opt.pars), ...)

  ghyp.object@parametrization <- "alpha.bar"
  ghyp.object@call <- call
  return(ghyp.object) 
}
#<--------------------------------   Gaussian   ------------------------------------>
"fit.gaussmv" <- function(data, na.rm = T, save.data = T)
{
  call <- match.call() 
  
  data.mat <- check.data(data, case = "mv", na.rm = na.rm, fit = TRUE)

  mu <- colMeans(data.mat, na.rm = na.rm)
  sigma <- var(data.mat, na.rm = na.rm)

  d <- length(mu)

  if(!save.data){
    data.mat <- NULL
  } 
  ghyp.object <- ghyp(lambda = as.numeric(NA), 
                      chi = Inf, 
                      psi = Inf,
                      alpha.bar = NULL,
                      mu = mu, 
                      sigma = sigma, 
                      gamma = rep(0, d),
                      data = data.mat)

  ghyp.object@parametrization <- "Gaussian"
  ghyp.object@call <- call

  llh <- sum(dghyp(data.mat, ghyp.object, logvalue = TRUE))

  aic <- -2 * llh + d/2 * (d + 1) + d 
  
  return(fit.ghyp(ghyp.object, llh = llh, n.iter = 0,
                  converged = TRUE,
                  error.code = 0, error.message = "",
                  parameter.variance = matrix(numeric(0)),
                  fitted.params = c(mu = TRUE, sigma = TRUE), aic = aic))
}
