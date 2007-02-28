"qqghyp" <- function(object,data=ghyp.data(object),
                   gaussian=T,line=T,
                   main="Generalized Hyperbolic Q-Q Plot",
                   xlab="Sample quantiles",ylab="Theoretical quantiles",
                   ghyp.pch = 1,gauss.pch=6,ghyp.lty="solid",
                   gauss.lty="dashed",ghyp.col="black",
                   gauss.col="black",
                   legend=T,location="topleft",legend.cex=0.8,
                   spline.points=150,
                   root.tol = .Machine$double.eps^0.5,
                   rel.tol = root.tol,abs.tol = root.tol^1.5,
                   ...)
{
  test.class.ghyp(object,case="ghypuv")
  
  data <- check.data(data,case="uv",na.rm=T,fit=TRUE,dim=1)
  
  ## compute quantiles   

  ghyp.p <- seq(1/length(data),1-1/length(data),length=length(data))

  ghyp.q <- qghyp(ghyp.p,object,method="splines",
                  spline.points=spline.points,root.tol=root.tol,
                  rel.tol=rel.tol,abs.tol=abs.tol)
  emp.q <- sort(data)

##   plot ghyp quantiles
  plot(emp.q,ghyp.q,xlab=xlab,ylab=ylab,pch=ghyp.pch,col=ghyp.col,
       main=main,...)

  if(gaussian==TRUE){
    gauss.q <- qnorm(ghyp.p,mean=mean(data),sd=sd(data))
    points(emp.q,gauss.q,pch=gauss.pch,col=gauss.col)
  }

  if(line==TRUE){
    abline(lm(ghyp.q ~ emp.q),lty=ghyp.lty,col=ghyp.col)
    if(gaussian==TRUE){
      abline(lm(gauss.q ~ emp.q),lty=gauss.lty,col=gauss.col)
    }
  }

 if(legend==TRUE){
  if(gaussian==TRUE){
    legend(location,legend=c(object@model,"Gaussian"),col=c(ghyp.col,gauss.col),
           lty=c(ghyp.lty,gauss.lty),cex=legend.cex,pch=c(ghyp.pch,gauss.pch))
  }else{
    legend(location,legend=object@model,col=ghyp.col,
           lty=ghyp.lty,cex=legend.cex,pch=ghyp.pch)
  }

 }
}