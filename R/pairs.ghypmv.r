"pairs.ghypmv" <- function(x,data=ghyp.data(x),main="'ghypmv' pairwise plot.",
                         nbins=30, gaussian = T, qq = T,
                         hist.col=c("white", topo.colors(40)),
                         spline.points=150,root.tol = .Machine$double.eps^0.5,
                         rel.tol = root.tol,abs.tol = root.tol^1.5,...)
{
  test.class.ghyp(x,case="ghypmv")

  data <- check.data(data,case="mv",na.rm=T,fit=TRUE,dim=x@dimension)

  nc <- length(x@mu)
  
  ## local function for the purpose to plot axes (copied from pairs.default)
  localAxis <- function(side, x, y, xpd, bg, col = NULL, main,oma, ...) {
      if (side%%2 == 1){
        Axis(x, side = side, xpd = NA, ...)
      }else{
        Axis(y, side = side, xpd = NA, ...)
      }
  }

  ## If not defined: define a sensible title
  if(main=="'ghypmv' pairwise plot."){
    if(qq & !is.null(colnames(data))){
      main <- paste(main,"\nColnames: ",
                    paste(colnames(data),collapse=", "),sep="")
    }
  }

  tmp.par <- par()
  plot.new()

  par(mfrow = dim(x@sigma), mar =   c(0, 0, 0, 0) + .1,oma =   c(0, 0, 4, 0) + 3)

  on.exit(suppressWarnings(par(tmp.par)))

  for(i in 1:nc){
    for(j in 1:nc){              
      par(mfg=c(i,j))
      if(i==j){
        if(qq){
          qqghyp(redim(x,i),data[,i],main="",legend=gaussian,
                 gaussian=gaussian,xaxt="n",yaxt="n", 
                  spline.points=spline.points,root.tol=root.tol,
                  rel.tol=rel.tol,abs.tol=abs.tol,...)
                
        }else{
          plot(NA,xlim=c(0,1),ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="",...)
          text(0.5,0.5,labels=colnames(data)[i])
        }
      }else{

        if(i > j){
          ## 2 dimensional histogramm from package 'gplots'
          hist2d(data[,c(j,i)],nbins=nbins,col=hist.col,xaxt="n",yaxt="n",
                 xlab="",ylab="",axes = T, frame.plot =T,bty="o" )
          box()
        }else{
            plot((data[,c(j,i)]),xaxt="n",yaxt="n",xlab="",ylab="",...)
        }
      }

      if (j == 1 && !(i%%2))
          Axis(data[, i], side=2,...)

      if (i == 1 && !(j%%2))
          Axis(data[, j], side=3, ...)

      if (i == nc && (j%%2))
          Axis(data[, j], side=1, ...)
      if (j == nc && (i%%2))
          Axis(data[, i], side=4, ...)
    }
  }
  title(main = main, outer = T)
}

setMethod("pairs", signature(x="ghypmv"),pairs.ghypmv)
