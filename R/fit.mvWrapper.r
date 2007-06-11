#<--------------------------------   hyp   ------------------------------------>
"fit.hypmv" <- function(data, opt.pars = c(alpha.bar=T,mu=T,sigma=T,gamma=T),...){
  if(!is.null(list(...)$lambda)){
    stop("Do not submit lambda! Lambda is defined as (dimension+1)/2.\n")
  }
  lambda <- (min(dim(data))+1)/2
  fit.ghypmv(data=data, lambda=lambda, opt.pars =c(lambda=F,opt.pars),...)
}
#<--------------------------------   NIG   ------------------------------------>
"fit.NIGmv" <- function(data, opt.pars = c(alpha.bar=T,mu=T,sigma=T,gamma=T),...){
  if(!is.null(list(...)$lambda)){
    stop("Do not submit lambda! Lambda is defined as -0.5.\n")
  }
  fit.ghypmv(data=data, lambda=-0.5, opt.pars =c(lambda=F,opt.pars),...)
}
#<--------------------------------   t   ------------------------------------>
"fit.tmv" <- function(data, nu = 4, opt.pars = c(lambda=T,mu=T,sigma=T,gamma=T),...){
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
  fit.ghypmv(data=data, lambda=-nu/2,alpha.bar=0, opt.pars =c(alpha.bar=F,opt.pars),...)
}
#<--------------------------------   VG   ------------------------------------>
"fit.VGmv" <- function(data, lambda=1,opt.pars = c(lambda=T,mu=T,sigma=T,gamma=T),...){
  if(lambda < 0){
    warning("lambda < 0, Student-t distribution is fitted instead!\n")
  }
  if(!is.null(list(...)$alpha.bar)){
    stop("Do not submit alpha.bar! alpha.bar is defined as 0.\n")
  }
  fit.ghypmv(data=data, lambda=lambda,alpha.bar=0, opt.pars =c(alpha.bar=F,opt.pars),...)
}