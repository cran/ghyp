"q.default" <- function(p,pdf,pdf.args,interval,p.lower,...){
  if(p>0 & p <1){
    dist.func <- function(x,pdf,p.args,p,p.lower,...){
      ret.val <- p.default(q=x,pdf=pdf,pdf.args=p.args,lower=p.lower,...)
      ret.val$value - p
    }
    return(uniroot(dist.func,interval=interval,pdf=pdf,
                   p.args=pdf.args,p=p,p.lower=p.lower,...))
  }else if(p==0){
    return(p.lower)
  }else if(p==1){
    return(+Inf)
  }else{
    return(NA)
  }
}

