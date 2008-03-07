"stepAIC.ghyp" <- function(data, dist = c("ghyp", "hyp", "NIG", "VG", "t", "gauss"),
                           symmetric = NULL, ...)
{
 call <- match.call(expand.dots = TRUE)
 dist <- match.arg(dist, several.ok = TRUE)
 
 with.gauss <- "gauss" %in% dist
 
 dist <- dist[!(dist == "gauss")]
 
 type <- "uv"
 tmp.data <- try(check.data(data, case = "uv", na.rm = T, fit = TRUE), silent = TRUE)
 if(class(tmp.data) == "try-error"){
   tmp.data <- try(check.data(data, case = "mv", na.rm = T, fit = TRUE), silent = TRUE)
   type <- "mv"
   if(class(tmp.data) == "try-error"){
     stop("Invalid data!")
   }   
 }
 if(is.null(symmetric)){
   symm <- c(FALSE, TRUE)
 }else{
    if(symmetric == FALSE){
      symm <- FALSE
    }else if(symmetric == TRUE){
      symm <- TRUE   
    }else{
      stop("Argument 'asymmetric' must be either 'NULL', 'TRUE' or 'FALSE'!")
   }
 }
 nbr.fits <- length(dist) * length(symm)
 function.names <- paste("fit.", dist, type, sep = "")
 fitted.objects <- vector(mode = "list", nbr.fits)

 fit.info <- data.frame(model = rep(dist, length(symm)),
                        symmetric = rep(symm, each = length(dist)),
                        lambda = rep(NA, nbr.fits),
                        alpha.bar = rep(NA, nbr.fits),
                        aic = rep(NA, nbr.fits),
                        llh = rep(NA, nbr.fits), 
                        converged = rep(NA, nbr.fits),
                        n.iter = rep(NA, nbr.fits),
                        stringsAsFactors = FALSE)

 ## In the univariate case return a data.frame with each ghyp parameter
 if(type=="uv"){
    uv.params <- data.frame(mu = rep(NA, nbr.fits), 
                            sigma = rep(NA, nbr.fits), 
                            gamma = rep(NA, nbr.fits),
                            stringsAsFactors = FALSE)
 }
                        
 for(j in 1:length(symm)){
   for(i in 1:length(function.names)){
     if(symm[j]){
       cat("Currently fitting: symmetric", dist[i], "\n")
     }else{
       cat("Currently fitting: asymmetric", dist[i], "\n")     
     }
     call.args <- c(list(data = tmp.data), list(...), list(symmetric = symm[j]))
    
     tmp.fit <- do.call(function.names[i], call.args)
    
     tmp.fit@call <- call

     tmp.params <- coef(tmp.fit, type = "alpha.bar")
     tmp.fit.info <- ghyp.fit.info(tmp.fit)

     if(type=="uv"){
       uv.params[(j - 1) * length(dist) + i, ] <- data.frame(tmp.params$mu, 
                                                             tmp.params$sigma, 
                                                             tmp.params$gamma,
                                                             stringsAsFactors = FALSE)
     }

     tmp.result <- data.frame(dist[i], symm[j], 
                              tmp.params$lambda, tmp.params$alpha.bar, 
                              tmp.fit.info$aic, tmp.fit.info$logLikelihood,
                              tmp.fit.info$converged, tmp.fit.info$n.iter,
                              stringsAsFactors = FALSE)
   
     fit.info[(j - 1) * length(dist) + i, ] <- tmp.result
     fitted.objects[[(j - 1) * length(dist) + i]] <- tmp.fit
   }
 }
 
 if(with.gauss){
   if(!is.null(list(...)$save.data)){
     save.data <- list(...)$save.data
   }else{
     save.data <- T
   }
   if(!is.null(list(...)$na.rm)){
     na.rm <- list(...)$na.rm
   }else{
     na.rm <- T
   }   
   cat("Currently fitting: gauss\n")
   if(type == "uv"){
      gauss.fit <- fit.gaussuv(tmp.data, save.data = save.data, na.rm = na.rm)
      gauss.uv.params <- data.frame(gauss.fit@mu, gauss.fit@sigma, 0, stringsAsFactors = FALSE)
      colnames(gauss.uv.params) <- colnames(uv.params)
      uv.params <- rbind(uv.params, gauss.uv.params)   
   }else{
      gauss.fit <- fit.gaussmv(tmp.data, save.data = save.data, na.rm = na.rm)   
   }

   gauss.fit.info <- data.frame("gauss", TRUE, NA, Inf, AIC(gauss.fit), 
                                logLik(gauss.fit), TRUE, 0, stringsAsFactors = FALSE)
                                
   colnames(gauss.fit.info) <- colnames(fit.info)
   fit.info <- rbind(fit.info, gauss.fit.info)   
   
   fitted.objects <- c(fitted.objects, gauss.fit)
 }
 if(type == "uv"){
   fit.info <- cbind(fit.info[, 1:4], uv.params, fit.info[, 5:8])
 } 
 
 idx <- which(fit.info$aic == min(fit.info$aic, na.rm = TRUE))
 if(length(idx) > 1){
   warning("Several AIC minima observed; the first minima will be returned!")
   idx <- idx[1]
 }
 best.model <- fitted.objects[[idx]]
 fit.info <- fit.info[order(fit.info$aic, na.last = TRUE), ]
 return(list(best.model = best.model, all.models = fitted.objects, fit.table = fit.info))
}
