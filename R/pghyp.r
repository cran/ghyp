"pghyp" <- function(q, object = ghyp(), n.sim = 10000, subdivisions = 200,
                    rel.tol = .Machine$double.eps^0.5, abs.tol = rel.tol)
{
  test.ghyp(object, case = "ghyp")
  q.raw  <- check.data(q, case = if(object@dimension > 1) "mv" else "uv",
                       na.rm = FALSE, fit = FALSE, dim = 1)

  if(is.univariate(object)){
    if(is.gaussian(object)){
      return(pnorm(q, mean = object@mu, sd = object@sigma))
    }else if(is.symmetric.t(object)){
       nu <- -2 * coef(object)$lambda
       return(pt((q - object@mu) / (sqrt((nu - 2) / nu) * object@sigma), df = nu))
    }else{
      q.finite <- q.raw[is.finite(q.raw)]
      p.raw <- rep(NA, length(q.raw))
      p.raw[q.raw == -Inf] <- 0
      p.raw[q.raw == Inf] <- 1
  
      q.mat <- matrix(q.finite, ncol = 1)
  
      pdf.args <- list(lambda = object@lambda, chi = object@chi, psi = object@psi,
                       mu = object@mu, sigma = object@sigma, gamma = object@gamma)
      
      value <- apply(q.mat, MARGIN = 1, FUN = p.default, pdf = "internal.dghyp",
                     lower = -Inf, pdf.args = pdf.args, subdivisions = subdivisions,
                     rel.tol = rel.tol, abs.tol = abs.tol)
      
      p.raw[is.finite(q.raw)] <- value
      return(p.raw)  
    }
  }else{
    sim.data <- rghyp(n.sim, object)
      eval.smaller.as <- function(q.raw, sim.data, n.sim)
      {
        return(sum(apply(apply(sim.data, MARGIN = 1, FUN = '<', q.raw),
                               MARGIN = 2, FUN = all)) / n.sim)
  
      }
    return(apply(q.raw, MARGIN = 1, FUN = eval.smaller.as, 
                 sim.data = sim.data, n.sim = n.sim))
  }
}

