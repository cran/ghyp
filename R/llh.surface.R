##param=c("alpha.bar","lambda","mu","sigma","gamma")
##param=c("sigma","gamma")
##param=c("s")
##param=c("s","mu")
##x.seq=NULL
##y.seq=NULL
##x.range=c(.50,1.5)
##y.range=c(0,1)
##n.grid=10
llh.surface <- function(obj,param=c("alpha.bar","lambda","mu","sigma","gamma"),
                        x.seq=NULL,y.seq=NULL,x.range=c(0,1),y.range=c(0,1),
                        n.grid=100, percent=FALSE,plot.it=TRUE)
{
  param <- match.arg(param,several.ok=TRUE)
  current.params <- coef(obj,type="alpha.bar")
  if(is.null(x.seq)){
    if(percent){
      v.0 <- current.params[[param[1]]]
      x.seq <- seq(v.0*min(x.range),v.0*max(x.range),length=n.grid)
    }else{
      x.seq <- seq(min(x.range),max(x.range),length=n.grid)
    }
  }
  x.seq <- sort(x.seq)
  if(is.null(y.seq) & length(param)>1){
    if(percent){
      v.0 <- current.params[[param[2]]]
      y.seq <- seq(v.0*min(y.range),v.0*max(y.range),length=n.grid)
    }else{
      y.seq <- seq(min(y.range),max(y.range),length=n.grid)
    }
  }
  y.seq <- sort(y.seq)  
  vectorized.llh <- function(tmp.params,data){
    sum(dghyp(data,do.call("ghyp",as.list(tmp.params)),logvalue=TRUE))
  }
    
  if(length(param)==1){
     param.mat <- matrix(unlist(current.params),ncol=5,nrow=length(x.seq),byrow=T,
                         dimnames=list(NULL,names(current.params)))
     param.mat[,which(colnames(param.mat)==param)] <- x.seq
     llh <- apply(param.mat,1,"vectorized.llh",data=ghyp.data(obj))
    if(plot.it){
      plot(x.seq,llh,xlab=param,main="Log-likelihood") 
    }else{
      return(list(x.seq,llh=llh))
    }
  }else if(length(param)==2){
     llh <- matrix(NA,ncol=length(y.seq),nrow=length(x.seq))
     for(i in 1:length(x.seq)){
       param.mat <- matrix(unlist(current.params),ncol=5,nrow=length(y.seq),byrow=T,
                           dimnames=list(NULL,names(current.params)))
       param.mat[,which(colnames(param.mat)==param[1])] <- x.seq[i]
       param.mat[,which(colnames(param.mat)==param[2])] <- y.seq       
       llh[i,] <- apply(param.mat,1,"vectorized.llh",data=ghyp.data(obj))
     }
    if(plot.it){
      persp(x=x.seq,y=y.seq,z=llh,theta = -30,phi=30)
    }else{
      return(llh)
    }
  }
  
}

##data(smi.stocks)
##x <- fit.tuv(smi.stocks[,1])
##llh.surface(x,param=c("mu","gamma"),n.grid=100,x.seq=c(1,2),y.seq=1:4)
##llh.surface(x,param=c("mu","gamma"),n.grid=10)
##llh.surface(x,param=c("sigma"),x.range=c(0.999,1.001),percent=T)
##llh.surface(x,param=c("lambda"),x.range=c(-3,-1.001))
##