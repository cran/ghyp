"pgig" <- function(q,lambda=1,chi=1,psi=1,...){
  check.gig.pars(lambda,chi,psi)
  q <- matrix(q,ncol=1)
  pdf.args <- list(lambda=lambda,chi=chi,psi=psi)
  p <- apply(q,MARGIN=1,FUN=p.default,pdf="dgig",lower=0,pdf.args=c(pdf.args,list(...)))
  value <- sapply(p,FUN=function(x)x$value)
  message <- sapply(p,FUN=function(x)x$message)
  int.err <- which(message!="OK")
  if(length(int.err)>0){
    value[int.err] <- NA
    warning(paste("Some integrations did fail!\nThe 'q' values are:",
    q[int.err]))
  }
  return(value)
}

