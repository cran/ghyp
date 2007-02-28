"fit.ghyp" <- function(object,llh=0,n.iter=0,converged=FALSE,error.code=0,
                     error.message="",parameter.variance)

{
  if(missing(parameter.variance)){
    return(new("mle.ghypmv",lambda=object@lambda,chi=object@chi,psi=object@psi,
                            alpha.bar=object@alpha.bar,mu=object@mu,
                            sigma=object@sigma,gamma=object@gamma,
                            model=object@model,dimension=object@dimension,
                            expected.value=object@expected.value,
                            variance=object@variance,data=object@data,
                            llh=llh,n.iter=n.iter,converged=converged,
                            error.code=error.code,error.message=error.message))

  }else{
    return(new("mle.ghypuv",lambda=object@lambda,chi=object@chi,psi=object@psi,
                            alpha.bar=object@alpha.bar,mu=object@mu,
                            sigma=object@sigma,gamma=object@gamma,
                            model=object@model,dimension=object@dimension,
                            expected.value=object@expected.value,
                            variance=object@variance,data=object@data,
                            llh=llh,n.iter=n.iter,converged=converged,
                            parameter.variance=parameter.variance,
                            error.code=error.code,error.message=error.message))
  }
}