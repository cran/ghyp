"hist.ghypuv"   <- function(x,data=ghyp.data(x),
                        gaussian=TRUE,log=F,ylim=NULL,
                        ghyp.col=1,ghyp.lwd=1,ghyp.lty="solid",
                        col=1,nclass=30,legend=TRUE,
                        location=if (log) "bottom" else "topright",
                        legend.cex=1,...)
{
  test.class.ghyp(x,"ghypuv")
  
  data <- check.data(data,case="uv",na.rm=T,fit=TRUE,dim=1)
  
  x.gh <- seq(min(data),max(data),length=2000)
  tmp.d.ghyp <- dghyp(x.gh,x)
 
  if(is.null(ylim)){
    ylim <- c(0,max(tmp.d.ghyp))
  }

  if(log==TRUE){
    tmp.hist <- hist(data,prob=T,plot=F,nclass=nclass,...)
    ghyp.data <- tmp.hist$breaks[2:length(tmp.hist$breaks)]-diff(tmp.hist$breaks)[1]/2
    Density <- tmp.hist$density
    plot(ghyp.data,log(Density),col=col,...)
    lines(x.gh,log(tmp.d.ghyp),col=ghyp.col,lwd=ghyp.lwd,lty=ghyp.lty)
    if(gaussian==TRUE){
      lines(x.gh,log(dnorm(x.gh,mean=mean(data),sd=sd(data))),col=col)
    }
  }else{
    hist(data,ylim=ylim,prob=T,nclass=nclass,...)
    lines(x.gh,tmp.d.ghyp,col=ghyp.col,lwd=ghyp.lwd,lty=ghyp.lty)
    if(gaussian==TRUE){
      lines(x.gh,dnorm(x.gh,mean=mean(data),sd=sd(data)),col=col)
    }
  }
  if(legend==TRUE){

    if(log==TRUE){
      if(gaussian==TRUE){
        tmp.text <- c("Histogramm",x@model,"Gaussian")
        tmp.col <- c(col,ghyp.col,col)
        tmp.lty <- c(NA,ghyp.lty,"dotted")
        tmp.pch <- c(1,NA,NA)
        legend(location,legend=tmp.text,col=tmp.col,
        lty=tmp.lty,pch=tmp.pch,cex=legend.cex)
      }else{
        tmp.text <- c("Histogramm",x@model)
        tmp.col <- c(col,ghyp.col)
        tmp.lty <- c(NA,ghyp.lty)
        tmp.pch <- c(1,NA)
        legend(location,legend=tmp.text,col=tmp.col,
        lty=tmp.lty,pch=tmp.pch,cex=legend.cex)
      }
    }else{
      if(gaussian==TRUE){
        tmp.text <- c(x@model,"Gaussian")
        tmp.col <- c(ghyp.col,col)
        tmp.lty <- c(ghyp.lty,"dotted")
        legend(location,legend=tmp.text,col=tmp.col,
        lty=tmp.lty,cex=legend.cex)
      }else{
        legend(location,legend=x@model,col=ghyp.col,
        lty=ghyp.lty,cex=legend.cex)
      }
    }
  }
}

setMethod("hist", signature(x="ghypuv"),hist.ghypuv)
