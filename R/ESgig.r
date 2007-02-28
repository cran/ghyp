"ESgig" <- function(p,lambda=1,chi=1,psi=1,...)
{
  check.gig.pars(lambda,chi,psi)

  value <- qgig(p=p,lambda,chi,psi)
  pdf.args <- list(lambda=lambda,chi=chi,psi=psi)
  value <- matrix(value,ncol=1)
  es.int.list <- apply(value,MARGIN=1,FUN=p.default,pdf="integrate.moment.gig",
                       lower=0,pdf.args=pdf.args)
  es.value <- sapply(es.int.list,FUN=function(x)x$value)
  es.value <- es.value/p
  return(es.value)
}
