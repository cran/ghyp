"lik.ratio.test" <- function(x, x.subclass, conf.level = 0.95)
{
  if(!is(x, "mle.ghyp") | !is(x.subclass, "mle.ghyp")){
    stop("Objects are not of class 'mle.ghyp'!")
  }

  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || 
      conf.level < 0 || conf.level > 1)){ 
        stop("'conf.level' must be a single number between 0 and 1")
  }

  df <- sum(x@fitted.params) - sum(x.subclass@fitted.params)

  if(df < 1){
    stop("Degree of freedom equals zero. Consult the help for more information!")
  }

  L.stat.chisq <- 2 * (logLik(x) - logLik(x.subclass))

  chisq.quantile <- qchisq(conf.level, df = df)
  
  p.value <- 1 - pchisq(L.stat.chisq, df = df)      # P(X > L.stat.chisq)

  L.stat <- exp(logLik(x.subclass)-logLik(x))

  return(list(statistic = c(L = L.stat), p.value = p.value, df = df,
              H0 = L.stat.chisq <= chisq.quantile))
}
