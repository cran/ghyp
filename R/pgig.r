"pgig" <- function(q, lambda = 1, chi = 1, psi = 1, ...)
{
  check.gig.pars(lambda, chi, psi)
  
  q.raw  <- q

  q.raw[q.raw < 0] <- NA
  q.raw[q.raw == 0] <- 0

  q <- q.raw[is.finite(q.raw) & q.raw > 0]
  if(length(q) == 0){
    return(q.raw)
  }
  
  q <- matrix(q, ncol = 1)
  pdf.args <- list(lambda = lambda, chi = chi, psi = psi)
  value <- apply(q, MARGIN = 1, FUN = p.default, pdf = "dgig", lower = 0,
                 pdf.args = c(pdf.args, list(...)))

  q.raw[is.finite(q.raw) & q.raw > 0] <- value
  return(q.raw)  
}

