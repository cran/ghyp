"stepAIC.ghyp" <- function(data,
                           dist=c("ghyp","hyp","NIG","VG","t"),
                           symmetric=NULL,...)
{
 dist <- match.arg(dist,several.ok=TRUE)
 tmp.data <- try(check.data(data,case="uv",na.rm=T,fit=TRUE),silent=TRUE)
 type <- "uv"
 if(class(tmp.data)=="try-error"){
   tmp.data <- try(check.data(data,case="mv",na.rm=T,fit=TRUE),silent=TRUE)
   type <- "mv"
   if(class(tmp.data)=="try-error"){
     stop("Invalid data!!!")
   }   
 }
 if(is.null(symmetric)){
   symm <- c(FALSE,TRUE)
 }else{
    if(symmetric==FALSE){
      symm <- FALSE
    }else if(symmetric==TRUE){
      symm <- TRUE   
    }else{
      stop("Argument 'asymmetric' must be in 'NULL', 'TRUE' or 'FALSE'!")
   }
 }
 nbr.fits <- length(dist)*length(symm)
 function.names <- paste("fit.",dist,type,sep="")
 fitted.objects <- vector(mode="list",nbr.fits)

 fit.info <- data.frame(model = rep(function.names,length(symm)),
                        symmetric = rep(symm,each=length(function.names)),
                        lambda = rep(NA,nbr.fits),
                        alpha.bar = rep(NA,nbr.fits),
                        aic = rep(NA,nbr.fits),
                        llh = rep(NA,nbr.fits), 
                        converged = rep(NA,nbr.fits),
                        n.iter = rep(NA,nbr.fits))

 ## In the univariate case return a data.frame with each ghyp parameter
 if(type=="uv"){
    uv.params <- data.frame(mu = rep(NA,nbr.fits), 
                            sigma = rep(NA,nbr.fits), 
                            gamma = rep(NA,nbr.fits))
 }
                        
 for(j in 1:length(symm)){
   for(i in 1:length(function.names)){
     if(symm[j]){
       cat("Currently fitting: symmetric",dist[i],"\n")
     }else{
       cat("Currently fitting: asymmetric",dist[i],"\n")     
     }
     call.args <- c(list(data=tmp.data),list(...),list(symmetric=symm[j]))
    
     tmp.fit <- do.call(function.names[i],call.args)
     fitted.objects[[(j-1)*length(dist)+i]] <- tmp.fit

     tmp.params <- ghyp.params(tmp.fit,type="alpha.bar")
     tmp.fit.info <- ghyp.fit.info(tmp.fit)

     if(type=="uv"){
       uv.params[(j-1)*length(dist)+i,] <- data.frame(tmp.params$mu, 
                                                      tmp.params$sigma, 
                                                      tmp.params$gamma)
     }

     tmp.result <- data.frame(function.names[i], symm[j], 
                     tmp.params$lambda, tmp.params$alpha.bar, 
                     tmp.fit.info$aic, tmp.fit.info$logLikelihood,
                     tmp.fit.info$converged, tmp.fit.info$n.iter)
     
     fit.info[(j-1)*length(dist)+i,] <- tmp.result

   }
 }

 if(type=="uv"){
   fit.info <- cbind(fit.info[,1:4],uv.params,fit.info[,5:8])
 } 
 
 idx <- which(fit.info$aic==min(fit.info$aic,na.rm=TRUE))
 if(length(idx)>1){
   warning("Several AIC minima observed; the first minima will be returned!")
   idx <- idx[1]
 }
 best.model <- fitted.objects[[idx]]
 fit.info <- fit.info[order(fit.info$aic),]
 return(list(best.model=best.model,all.models=fitted.objects,fit.table=fit.info))
}
