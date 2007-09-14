#<--------------------------------   hyp   ------------------------------------>
"fit.hypuv" <- function(data, opt.pars = c(alpha.bar = T, mu = T, sigma = T, gamma = !symmetric),
                        symmetric = F, ...)
{
  call <- match.call(expand.dots = TRUE) 

  if(!is.null(list(...)$lambda)){
    stop("Do not submit lambda! Lambda is defined as 1.\n")
  }
  ghyp.object <- fit.ghypuv(data = data, lambda = 1, 
                            opt.pars = c(lambda = F, opt.pars),
                            symmetric = symmetric, ...)

  ghyp.object@parametrization <- "lambda.alpha.bar"
  ghyp.object@call <- call
  return(ghyp.object) 
}
#<--------------------------------   NIG   ------------------------------------>
"fit.NIGuv" <- function(data, opt.pars = c(alpha.bar = T, mu = T, sigma = T, gamma = !symmetric),
                        symmetric = F, ...)
{
  call <- match.call(expand.dots = TRUE) 
  
  if(!is.null(list(...)$lambda)){
    stop("Do not submit lambda! Lambda is defined as -0.5.\n")
  }
  ghyp.object <- fit.ghypuv(data = data, lambda = -0.5, 
                            opt.pars = c(lambda = F, opt.pars),
                            symmetric = symmetric, ...)

  ghyp.object@parametrization <- "lambda.alpha.bar"
  ghyp.object@call <- call
  return(ghyp.object) 
}
#<--------------------------------   Student-t   ------------------------------------>
"fit.tuv" <- function(data, nu = 3.5, opt.pars = c(nu = T, mu = T, sigma = T, gamma = !symmetric),
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
  ghyp.object <- fit.ghypuv(data = data, lambda = -nu/2, alpha.bar = 0, 
                            opt.pars = c(alpha.bar = F, opt.pars),
                            symmetric = symmetric, ...)

  ghyp.object@parametrization <- "lambda.alpha.bar"
  ghyp.object@call <- call
  return(ghyp.object) 
}
#<--------------------------------   VG   ------------------------------------>
"fit.VGuv" <- function(data, lambda = 1, opt.pars = c(lambda = T, mu = T, sigma = T, gamma = !symmetric),
                       symmetric = F, ...)
{
  call <- match.call(expand.dots = TRUE) 
  
  if(lambda < 0){
    warning("lambda < 0, Student-t distribution is fitted instead!\n")
  }
  if(!is.null(list(...)$alpha.bar)){
    stop("Do not submit alpha.bar! alpha.bar is defined as 0.\n")
  }
  ghyp.object <- fit.ghypuv(data = data, lambda = lambda, alpha.bar = 0, 
                            opt.pars = c(alpha.bar = F, opt.pars),
                            symmetric = symmetric, ...)

  ghyp.object@parametrization <- "lambda.alpha.bar"
  ghyp.object@call <- call
  return(ghyp.object) 
}
