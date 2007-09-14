"ESghyp" <- function(p, object = ghyp(), ...)
{
  test.ghyp(object, case = "univariate")
  p <- check.data(p, na.rm = FALSE, fit = FALSE, dim = 1)
  
  value <- qghyp(p, object,...)
  
  pdf.args <- list(lambda = object@lambda, chi = object@chi, psi = object@psi,
                   mu = object@mu, sigma = object@sigma, gamma = object@gamma)
                   
  value <- matrix(value, ncol = 1)
  es.int.list <- apply(value, MARGIN = 1, FUN = p.default, 
                       pdf = "integrate.moment.ghypuv",
                       lower = -Inf, pdf.args = pdf.args)
  es.value <- sapply(es.int.list, FUN = function(x)x$value)
  es.value <- es.value/p
  return(es.value)
} 
