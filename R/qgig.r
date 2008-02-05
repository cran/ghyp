"qgig" <- function(p,lambda=1,chi=1,psi=1,method=c("integration","splines"),
                 spline.points=200, subdivisions=200, root.tol = .Machine$double.eps^0.5,
                 rel.tol = root.tol^1.5, abs.tol = rel.tol,...)

{
  p.raw  <- p

  method <- match.arg(method)

  p.raw[p.raw<0 | p.raw>1] <- NaN
  p.raw[p.raw==1] <- Inf
  p.raw[p.raw==0] <- 0

  ## If only !is.finite quantiles are passed return NA, NaN, Inf, -Inf
  p <- p.raw[is.finite(p.raw)]
  if(length(p)==0){
    return(p.raw)
  }
  #<----   Use Newton's method to find the range of the quantiles ---->
  internal.newton <- function(lambda,chi,psi,p,tol,rel.tol,abs.tol,subdivisions)
  {
    newton.tol <- 1
    iter <- 0
    x0 <- Egig(lambda,chi,psi,func="x")
    while(newton.tol > tol & iter < 100){
      iter <- iter + 1
      F1 <- pgig(q=x0,lambda,chi,psi,rel.tol=rel.tol,abs.tol=abs.tol,
                            subdivisions=subdivisions) - p
      x1 <- x0 - F1/dgig(x0,lambda,chi,psi)
      newton.tol <- abs((x1-x0)/x0)
      x0 <- x1
      ##cat("x0: ",x0, "  F1: ", F1, " newton.tol", newton.tol,"\n")
      if(is.nan(newton.tol)){
        stop("Unable to determine interval where the quantiles are in-between!") 
      }      
    }
    return(x0)
  }
  #<---------------- end of Newton iteration  ---------------------->
  gig.expected <- Egig(lambda,chi,psi,func="x")
  q.expected <- pgig(gig.expected,lambda,chi,psi,
                     subdivisions=subdivisions,rel.tol=rel.tol, abs.tol= abs.tol)
  if(q.expected > max(p)){
    interval <- c(0,gig.expected)
  }else{
    interval.max <- internal.newton(lambda,chi,psi,max(p),
                                    root.tol,rel.tol,abs.tol,subdivisions)
    interval <- c(0,interval.max*1.1)                                
  }
  if(method=="integration"){
    ## The integration method 
    pdf.args <- list(lambda=lambda,chi=chi,psi=psi)
    p <- matrix(p,ncol=1)
    q.val <- apply(p,MARGIN=1,FUN=q.default,pdf="dgig",
                   pdf.args=pdf.args,
                   interval=interval,tol=root.tol,p.lower=0,
                   rel.tol=rel.tol,abs.tol=abs.tol,subdivisions=subdivisions)
    value <- sapply(q.val,FUN=function(x)x$root)
  }else{
    ## The spline method 
    interval.seq <- seq(min(interval),max(interval),length=spline.points)
    ## Compute the distribution function to be interpolated by splines
    p.interval <- pgig(q=interval.seq,lambda=lambda,chi=chi,psi=psi,rel.tol=rel.tol,
                       abs.tol=abs.tol,subdivisions=subdivisions)
    ## Spline function
    spline.distribution.func <- splinefun(interval.seq,p.interval)

    ## root function:   condition: quantile.root.func == 0
    quantile.root.func <- function(x,tmp.p){
      spline.distribution.func(x)-tmp.p   
    }
    value <- p
    for(i in 1:length(p)){
      value[i] <- uniroot(quantile.root.func,interval=interval,
                          tmp.p=p[i],tol=root.tol)$root
    }
    value <- as.vector(value)
  }
  p.raw[is.finite(p.raw)] <- value
  return(p.raw)
}
