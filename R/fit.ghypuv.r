"fit.ghypuv" <- function(data,lambda=1, alpha.bar=.1, mu=mean(data), 
                         sigma=sqrt(var(data)), gamma=0, 
                         opt.pars = c(lambda=T, alpha.bar = T,mu=T,sigma=T,gamma=!symmetric),
                         symmetric=F,save.data=T,na.rm=T,silent=FALSE,...)
{
  
  
  #<------------ Check input ---------------->
  opt.pars <- check.opt.pars(opt.pars)
  data <- check.data(data=data,case="uv",na.rm=na.rm)

  check.norm.pars(mu,sigma,gamma)

  tmp.abar2chipsi <- abar2chipsi(alpha.bar,lambda)
  chi <- tmp.abar2chipsi$chi
  psi <- tmp.abar2chipsi$psi
  #<----- check parameters of the mixing distribution for consistency --------->
  check.gig.pars(lambda,chi,psi)
  
  var.names <- c("lambda","alpha.bar",  "mu", "sigma","gamma")

  vars <- unname(c(lambda, alpha.bar,  mu, sigma, gamma))
  names(vars) <- var.names
  
  if(alpha.bar==0 & opt.pars["alpha.bar"]==F){
    if(lambda>0){
       transform <- c("abs","abs","identity","abs","identity")
    }else{
       transform <- c("t.transform","abs","identity","abs","identity")
    }
  }else{
    transform <- c("identity","abs","identity","abs","identity")
  }
  
  
  names(transform) <- var.names
        
  tmp.fit <- mle.default(data=data,pdf="internal.dghyp",
              vars=vars,
              opt.pars = opt.pars,
              transform=transform,se = T,
              silent=silent,...)

   if(tmp.fit$convergence!=0){
     converged <- FALSE
   }else{
     converged <- TRUE
   }

   if(is.null(tmp.fit$message)){
     conv.type <- ""
   }else{
     conv.type <- paste("Message from 'optim':",tmp.fit$message)
   }

   if(save.data==FALSE){
     data <- numeric(0)
   }
   tmp.ghyp.object <- ghyp(lambda=tmp.fit$par.ests["lambda"],
                           alpha.bar=tmp.fit$par.ests["alpha.bar"],
                           mu=tmp.fit$par.ests["mu"],
                           sigma=tmp.fit$par.ests["sigma"],
                           gamma=tmp.fit$par.ests["gamma"],
                           data=data)

   return(fit.ghyp(tmp.ghyp.object,llh=tmp.fit$ll.max,n.iter=tmp.fit$n.iter,
                   converged=converged,
                   error.code=tmp.fit$convergence, error.message=conv.type,
                   parameter.variance=tmp.fit$parameter.variance))
}