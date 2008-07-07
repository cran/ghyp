"pghyp" <- function(q, object = ghyp(), n.sim = 10000, subdivisions = 200,
                    rel.tol = .Machine$double.eps^0.5, abs.tol = rel.tol,
                    lower.tail = TRUE)
{
  test.ghyp(object, case = "ghyp")
  q.raw  <- check.data(q, case = if(object@dimension > 1) "mv" else "uv",
                       na.rm = FALSE, fit = FALSE, dim = 1)

  if(is.univariate(object)){
    if(is.gaussian(object)){
      return(pnorm(q, mean = object@mu, sd = as.vector(object@sigma), lower.tail = lower.tail))
    }else if(is.student.t(object, symmetric = TRUE)){
       nu <- coef(object)$nu
       return(pt((q - object@mu) / (sqrt((nu - 2) / nu) * as.vector(object@sigma)), df = nu, 
                 lower.tail = lower.tail))
    }else{
      q.finite <- q.raw[is.finite(q.raw)]
      q.mat <- matrix(q.finite, ncol = 1)
      
      p.raw <- rep(NA, length(q.raw))

      pdf.args <- list(lambda = object@lambda, chi = object@chi, psi = object@psi,
                       mu = object@mu, sigma = object@sigma, gamma = object@gamma)


      if(lower.tail){
        p.raw[q.raw == -Inf] <- 0
        p.raw[q.raw == Inf] <- 1
        value <- apply(q.mat, MARGIN = 1, FUN = p.default, pdf = "internal.dghyp",
                       lower = -Inf, pdf.args = pdf.args, subdivisions = subdivisions,
                       rel.tol = rel.tol, abs.tol = abs.tol)
      }else{
        p.raw[q.raw == -Inf] <- 1
        p.raw[q.raw == Inf] <- 0
        value <- apply(q.mat, MARGIN = 1, FUN = p.default, pdf = "internal.dghyp",
                       upper = Inf, pdf.args = pdf.args, subdivisions = subdivisions,
                       rel.tol = rel.tol, abs.tol = abs.tol)
      }
      
      p.raw[is.finite(q.raw)] <- value
      return(as.vector(p.raw))
    }
  }else{
    sim.data <- rghyp(n.sim, object)
    if(lower.tail){
      compare.fun <- '<'
    }else{
      compare.fun <- '>'
    }
      
      eval.comparison <- function(q.raw, sim.data, n.sim, tmp.compare.fun)
      {
        return(sum(apply(apply(sim.data, MARGIN = 1, FUN = tmp.compare.fun, q.raw),
                               MARGIN = 2, FUN = all)) / n.sim)
  
      }
    
    return(apply(q.raw, MARGIN = 1, FUN = eval.comparison, 
                 tmp.compare.fun = compare.fun, sim.data = sim.data, n.sim = n.sim))
  }
}

