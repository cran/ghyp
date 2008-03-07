"ESgig" <- function(alpha, lambda = 1, chi = 1, psi = 1, distr = c("return", "loss"), ...)
{
  distr <- match.arg(distr)

  check.gig.pars(lambda, chi, psi)

  value.raw <- qgig(alpha, lambda, chi, psi, ...)

  if(all(is.na(value.raw))){
    return(value.raw)
  }
  
  pdf.args <- list(lambda = lambda, chi = chi, psi = psi)
  
  value.es <- matrix(value.raw[!is.na(value.raw)], ncol = 1)

  if(distr == "return"){
    value.es <- apply(value.es, MARGIN = 1, FUN = p.default, pdf = "integrate.moment.gig",
                      lower = 0, pdf.args = pdf.args)
    value.es <- value.es / alpha
  }else{
    value.es <- apply(value.es, MARGIN = 1, FUN = p.default, pdf = "integrate.moment.gig",
                      upper = Inf, pdf.args = pdf.args)
    value.es <- value.es / (1 - alpha)
  }

  value.raw[!is.na(value.raw)] <- value.es
  
  return(value.es)
}
