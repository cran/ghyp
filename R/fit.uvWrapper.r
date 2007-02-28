#<--------------------------------   hyp   ------------------------------------>
"fit.hypuv" <- function(data, opt.pars = c(alpha.bar=T,mu=T,sigma=T,gamma=!symmetric),
                        symmetric=F,...){
  if(!is.null(list(...)$lambda)){
    stop("Do not submit lambda! Lambda is defined as 1.\n")
  }
  fit.ghypuv(data=data, lambda=1, opt.pars =c(lambda=F,opt.pars),symmetric=symmetric,...)
}
#<--------------------------------   NIG   ------------------------------------>
"fit.NIGuv" <- function(data, opt.pars = c(alpha.bar=T,mu=T,sigma=T,gamma=!symmetric),
                        symmetric=F,...){
  if(!is.null(list(...)$lambda)){
    stop("Do not submit lambda! Lambda is defined as -0.5.\n")
  }
  fit.ghypuv(data=data, lambda=-0.5, opt.pars =c(lambda=F,opt.pars),symmetric=symmetric,...)
}
#<--------------------------------   t   ------------------------------------>
"fit.tuv" <- function(data, nu = 4, opt.pars = c(nu=T,mu=T,sigma=T,gamma=!symmetric),
                      symmetric=F,...){
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
  fit.ghypuv(data=data, lambda=-nu/2,alpha.bar=0, opt.pars =c(alpha.bar=F,opt.pars),
             symmetric=symmetric,...)
}
#<--------------------------------   VG   ------------------------------------>
"fit.VGuv" <- function(data, lambda=1,opt.pars = c(lambda=T,mu=T,sigma=T,gamma=!symmetric),
                       symmetric=F,...){
  if(lambda < 0){
    warning("lambda < 0, Student-t distribution is fitted instead!\n")
  }
  if(!is.null(list(...)$alpha.bar)){
    stop("Do not submit alpha.bar! alpha.bar is defined as 0.\n")
  }
  fit.ghypuv(data=data, lambda=lambda,alpha.bar=0, opt.pars =c(alpha.bar=F,opt.pars),
             symmetric=symmetric,...)
}
