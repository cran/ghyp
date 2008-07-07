"ESghyp" <- function(alpha, object = ghyp(), distr = c("return", "loss"), ...)
{
  distr <- match.arg(distr)

  test.ghyp(object, case = "univariate")

  alpha <- check.data(alpha, na.rm = FALSE, fit = FALSE, dim = 1)

  if(is.gaussian(object)){
    if(distr == "return"){
      return(object@mu - as.vector(object@sigma) * dnorm(qnorm(1 - alpha)) / (alpha))
    }else{
      return(object@mu + as.vector(object@sigma) * dnorm(qnorm(alpha)) / (1 - alpha)) # For losses
    }

  }else if(is.student.t(object, symmetric = TRUE)){
    nu <- coef(object)$nu
    sigma.t <- sqrt((nu - 2) / nu) * as.vector(object@sigma)
    if(distr == "return"){
      alpha <- 1 - alpha
      return(object@mu - sigma.t * dt(qt(alpha, df = nu), df = nu) / (1 - alpha) * (nu + qt(alpha, df = nu)^2) / (nu - 1))
    }else{
      return(object@mu + sigma.t * dt(qt(alpha, df = nu), df = nu) / (1 - alpha) * (nu + qt(alpha, df = nu)^2) / (nu - 1))
    }
  }else{
    value.raw <- qghyp(alpha, object,...)

    if(all(is.na(value.raw))){
      return(value.raw)
    }

    pdf.args <- list(lambda = object@lambda, chi = object@chi, psi = object@psi,
                     mu = object@mu, sigma = object@sigma, gamma = object@gamma)

    value.es <- matrix(value.raw[!is.na(value.raw)], ncol = 1)

    if(distr == "return"){
      value.es <- apply(value.es, MARGIN = 1, FUN = p.default,
                        pdf = "integrate.moment.ghypuv",
                        lower = -Inf, pdf.args = pdf.args)
      value.es <- value.es / alpha
    }else{
      value.es <- apply(value.es, MARGIN = 1, FUN = p.default,
                        pdf = "integrate.moment.ghypuv",
                        upper = Inf, pdf.args = pdf.args)
      value.es <- value.es / (1 - alpha)
    }

    value.raw[!is.na(value.raw)] <- value.es

    return(value.es)
  }
}
