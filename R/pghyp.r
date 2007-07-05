"pghyp" <- function(q,object=ghyp(),n.sim=10000, subdivisions=200,
                  rel.tol = .Machine$double.eps^0.5, abs.tol = rel.tol)
{
  test.class.ghyp(object,case="ghypbase")

  q  <- check.data(q,case = if(object@dimension>1)"mv" else "uv",
                   na.rm=FALSE,fit=FALSE,dim=1)

  if(is(object, "ghypuv")){
    q <- matrix(q,ncol=1)
    pdf.args <- list(lambda=object@lambda,chi=object@chi,psi=object@psi,mu=object@mu,
                          sigma=object@sigma,gamma=object@gamma)
    p <- apply(q,MARGIN=1,FUN=p.default,pdf="internal.dghyp",lower=-Inf,pdf.args=pdf.args,
               subdivisions=subdivisions,rel.tol=rel.tol,abs.tol=abs.tol)
    value <- sapply(p,FUN=function(x)x$value)
    message <- sapply(p,FUN=function(x)x$message)
    int.err <- which(message!="OK")
    if(length(int.err)>0){
      value[int.err] <- NA
      warning(paste("pghyp: Some integrations did fail!\nThe 'q' values are:",
      q[int.err]))
    }
    return(value)  
  }else{

    sim.data <- rghyp(n.sim,object)

    eval.smaller.as <- function(q,sim.data,n.sim){
      return(sum(apply(apply(sim.data  ,MARGIN=1,FUN = '<',q),
                             MARGIN=2,FUN=all))/n.sim)

    }
    return(apply(q,MARGIN=1,FUN=eval.smaller.as,sim.data=sim.data,n.sim=n.sim))
  }
}

