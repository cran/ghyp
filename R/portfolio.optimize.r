"portfolio.optimize" <- function(object, ptf.mean = 0.01,
                                 risk.measure = c("variance", "quantile", "expected-shortfall"),
                                 level = 0.95, ...)
{
  ## Object must be of class "ghyp" or "mle.ghyp" and multivariate
  test.ghyp(object, case = "multivariate")
  
  risk.measure <- match.arg(risk.measure)
  
  if(risk.measure=="variance"){
    lin.sys <- rbind(vcov(object), mean(object), rep(1,object@dimension))
    lin.sys <- cbind(lin.sys, c(mean(object), 0, 0), c(rep(1, object@dimension), 0, 0))
    b <- c(rep(0, object@dimension), ptf.mean, 1)
    ptf.weights <- solve(lin.sys, b)[1:object@dimension]
    tmp.portfolio <- transform(object, multiplier = t(ptf.weights))
    return(list(risk.measure = risk.measure,
                value = as.numeric(ptf.weights %*% vcov(object) %*% ptf.weights),
                portfolio = tmp.portfolio,
                opt.weights = unname(ptf.weights)))
  }else{
   if(object@dimension <= 2){
    stop("Dimension must be > 2!")
   }
   if(risk.measure == "quantile"){
     minimize.func <- qghyp
   }else{
     minimize.func <- ESghyp
   }
   
   objective <- function(opt.weights, object, minimize.func, ptf.mean, level){
     weight.2 <- (ptf.mean + mean(object)[1] * (sum(opt.weights) - 1) -
                  sum(opt.weights * mean(object)[3:object@dimension])) /
                  (mean(object)[2] - mean(object)[1])  
     weight.1 <- 1 - sum(opt.weights) - weight.2
     tmp.weights <- c(weight.1, weight.2, opt.weights)
     tmp.portfolio <- transform(object, multiplier = t(tmp.weights))
     objective.value <- minimize.func(1 - level, object = tmp.portfolio)
     ##cat("objective.value: ",objective.value,"\n")
     return( - objective.value )
   }
   
   initial.weights <- rep(1 / (object@dimension - 2), object@dimension - 2)
   opt.ptf <- optim(initial.weights, objective, object = object,
                    minimize.func = minimize.func, ptf.mean = ptf.mean, level = level, ...)

   weight.2 <- (ptf.mean + mean(object)[1] * (sum(opt.ptf$par) - 1) -
                sum(opt.ptf$par * mean(object)[3:object@dimension])) /
                (mean(object)[2] - mean(object)[1])
   weight.1 <- 1 - sum(opt.ptf$par) - weight.2
   tmp.weights <- c(weight.1, weight.2, opt.ptf$par)
   tmp.portfolio <- transform(object, multiplier = t(tmp.weights))

   return(list(portfolio = tmp.portfolio, risk.measure = risk.measure, 
               value = -opt.ptf$value, opt.weights = unname(tmp.weights),
               convergence = opt.ptf$convergence, message = opt.ptf$message,
               n.iter = opt.ptf$counts[1]))
  }
}
