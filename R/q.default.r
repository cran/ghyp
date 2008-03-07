"q.default" <- function(p, pdf, pdf.args, interval, p.lower, ...){
  if(p > 0 & p < 1){
    dist.func <- function(x, pdf, p.args, p, p.lower, ...){
      ret.val <- p.default(q = x, pdf = pdf, pdf.args = p.args, lower = p.lower, ...)
      return(ret.val - p)
    }
    tmp.quantile <- try(uniroot(dist.func, interval = interval, pdf = pdf,
                                p.args = pdf.args, p = p, p.lower = p.lower, ...))
    if(class(tmp.quantile) == "try-error"){
      warning("Failed to determine quantile with 'probs = ", p,
              "'!\nMessage: ", as.character(tmp.quantile), "\n")
      return(NA)    
    }else{
      return(tmp.quantile$root)
    }                            
  }else if(p == 0){
    return(p.lower)
  }else if(p == 1){
    return(+Inf)
  }else{
    return(NA)
  }
}

