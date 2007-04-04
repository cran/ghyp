"ghyp" <- function(lambda=0.5,chi=0.5,psi=2,mu=0,sigma=1,gamma=0,alpha.bar=NULL,data=NULL)
{
  if(!is.null(alpha.bar)){
    tmp.abar2chipsi <- abar2chipsi(alpha.bar,lambda)
    chi  <- tmp.abar2chipsi$chi
    psi  <- tmp.abar2chipsi$psi
    names(chi) <- "chi"
    names(psi) <- "psi"
  }else{
    alpha.bar <- sqrt(chi*psi)
    names(alpha.bar) <- "alpha.bar"
  }
  check.gig.pars(lambda,chi,psi)

  e.gig <- Egig(lambda,chi,psi,func="x")
  var.gig <- Egig(lambda,chi,psi,func="var")
  
  if(!is.numeric(gamma)){
    stop("ghyp: gamma must be numeric!\n")
  }
  if(!is.numeric(sigma)){
    stop("ghyp: sigma must be numeric!\n")
  }
  if(!is.numeric(mu)){
    stop("ghyp: mu must be numeric!\n")
  }

  model <- gh.model(lambda,chi,psi,gamma)
  if(length(mu)==1){
    #<----------   univariate case --------------->
    if(length(gamma)!= 1){
      stop("Gamma must be scalar!\n")
    }
    if(length(sigma)!=1){
      stop("Sigma must be scalar!\n")
    }
    if(length(mu)!=1){
      stop("Mu must be scalar!\n")
    }

    if(!is.null(data)){
      data <- check.data(data=data,case="uv",na.rm=F,fit=F)
    }else{
      data <- numeric(0)
    }
    return(new("ghypuv",lambda=lambda,chi=chi,psi=psi,alpha.bar=alpha.bar,
                      mu=mu,sigma=sigma,gamma=gamma,model=model,
                      dimension=1,expected.value=unname(mu+e.gig*gamma),
                      variance=unname(var.gig*gamma^2+e.gig*sigma^2),data=data))
  }else if(length(mu)>1){
      #<----------   multivariate case --------------->
      if(ncol(sigma)!=length(mu)){
        stop("Dimension mismatch ( ncol(sigma)!=length(mu) )!\n")
      }
      if(length(gamma)!=length(mu)){
        stop("Dimension mismatch ( length(gamma)!=length(mu) )!\n")
      }
      if(!is.null(data)){
        data <- check.data(data=data,case="mv",na.rm=F,fit=F)
      }else{
        data <- matrix(numeric(0))
      }

      return(new("ghypmv",lambda=lambda,chi=chi,psi=psi,alpha.bar=alpha.bar,
                        mu=mu,sigma=sigma,gamma=gamma,model=model,
                        dimension=length(mu),expected.value=mu+e.gig*gamma,
                        variance=var.gig*outer(gamma,gamma)+e.gig*sigma,data=data))

  }else{
    stop("Invalid parameters!\n")
  }
}