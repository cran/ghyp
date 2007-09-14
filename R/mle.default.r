"mle.default" <- function(data, pdf, vars, opt.pars = rep(TRUE, length(vars)), 
                          transform = NULL, se = FALSE, na.rm = FALSE, 
                          silent = FALSE, ...)
{

  if(na.rm){
    data <- data[!is.na(data)]
  }else{
    if(any(is.na(data))){
      stop("mle.default: The sample contains NAs!\n",
           "Set na.rm = TRUE to remove the rows which contain NAs.\n")
    }
  }
  ## Sort opt.pars according to vars (-> both vectors must be named)
  opt.pars <- opt.pars[match(names(vars),names(opt.pars))]


  ## Theta contains the parameters intended to be fitted
  theta = vars[opt.pars]

  ##<------------   Negative log-Likelihood function adapter ----------->
  negloglik <- function(theta, pdf, data, transf, const.pars, silent)
  {
    ## Transformation of the parameters
    for(nam in intersect(names(theta), names(transf))) {
      theta[nam] = do.call(transf[nam], list(theta[nam]))
    }
    pdf.args = c(list(x = data, logvalue = T), as.list(theta), as.list(const.pars))
    llh <- -sum(do.call(pdf, pdf.args))
    if(!silent){
      print(paste("Llh: ",sprintf("% .14E", -llh), "; Pars: ",
                  paste(sprintf("% .6E", theta), collapse = ", "),
                  sep = ""))
    }
    return(llh)
  }
  ##<------------------------------------------------------------------->
  
  fit = try(optim(theta, negloglik, hessian = se, pdf = pdf,
                  data = data, transf = transform, const.pars = vars[!opt.pars], silent = silent, ...))
  ##1  indicates that the iteration limit maxit had been reached.
  ##10 indicates degeneracy of the Nelder–Mead simplex.
  ##51 indicates a warning from the "L-BFGS-B" method; see component message for further details.
  ##52 indicates an error from the "L-BFGS-B" method; see component message for further details.
  if(class(fit) == "try-error") {
    warning("An error occured during the fitting procedure!")
    convergence = 100
    vars[1:length(vars)] = NA
    par.ests = vars
    alt.pars = vars
    par.ses = NA
    hess = NA
    ll.max = NA
    n.iter = NA
    message = fit 
  }else{
    par.ests <- fit$par
    names(par.ests) = names(theta)
    for(nam in intersect(names(par.ests), names(transform))) {
      par.ests[nam] = do.call(transform[nam], list(par.ests[nam]))
    }
    vars[opt.pars] = par.ests
    convergence = fit$convergence
    n.iter = fit$counts[1]
    ll.max = - fit$value
    message = NULL
    if(se) {
       hess = fit$hessian
       par.ses <- suppressWarnings(sqrt(diag(hess)))
       inv.hess <- try(solve(hess))
       if(class(inv.hess) == "try-error") {
         warning("Hessian matrix is singular!")
         inv.hess <- matrix(NA, ncol(hess), ncol(hess), dimnames = dimnames(hess))
       }
       names(par.ses) <- names(par.ests)
       dimnames(hess) <- list(names(par.ests), names(par.ests))
    }else{
       par.ses <- NA
       hess <- NA
    }
  }
  list(convergence = convergence, par.ests = vars, 
      parameter.variance = inv.hess, ll.max = ll.max, n.iter = n.iter,
      message=message)
}
