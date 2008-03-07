"qgig" <- function(p, lambda = 1, chi = 1, psi = 1, method = c("integration","splines"),
                   spline.points = 200, subdivisions = 200, root.tol = .Machine$double.eps^0.5,
                   rel.tol = root.tol^1.5, abs.tol = rel.tol, ...)

{
  p.raw  <- p

  method <- match.arg(method)

  p.raw[p.raw < 0 | p.raw > 1] <- NaN
  p.raw[p.raw == 1] <- Inf
  p.raw[p.raw == 0] <- 0

  ## If only !is.finite quantiles are passed return NA, NaN, Inf, -Inf
  p <- p.raw[is.finite(p.raw)]
  if(length(p) == 0){
    return(p.raw)
  }
  #<----   Use Newton's method to find the range of the quantiles ---->
  internal.bisection <- function(lambda, chi, psi, p, tol, rel.tol, abs.tol, subdivisions)
  {

    iter <- 0
    range.found <- FALSE
    step.size <- sqrt(Egig(lambda, chi, psi, func = "var"))

# ---------> workarround
    if(is.na(step.size)){
      step.size <- 0.1
    }
# ---------> end workarround

    q.0 <- Egig(lambda, chi, psi, func = "x")
    q.upper <- q.0 + step.size

    while(!range.found & iter < 100){
      iter <- iter + 1
      p.upper <- pgig(q = q.upper, lambda, chi, psi, rel.tol = rel.tol, abs.tol = abs.tol,
                      subdivisions = subdivisions) - p
      
      if(any(is.na(p.upper))){
        warning("Unable to determine interval where the quantiles are in-between.") 
        return(NA)              
      } 
##      cat("upper : ", p.upper, " q.upper: ", q.upper, "\n")
      if(p.upper <= 0){
        q.upper <- q.upper + step.size 
        next
      }
      if(p.upper > 0){
        range.found <- TRUE
      }
    }
    if(iter >= 100){
      warning("Unable to determine interval where the quantiles are in-between.") 
    }
    
    pdf.args <- list(lambda = lambda, chi = chi, psi = psi)

    q.root <- q.default(p, pdf = "dgig", pdf.args = pdf.args, 
                        interval = c(0 + .Machine$double.eps, q.upper), tol = root.tol,
                        p.lower = 0, rel.tol = rel.tol, abs.tol = abs.tol,
                        subdivisions = subdivisions)

    return(q.root)
  }
  #<---------------- end of Newton iteration  ---------------------->
  gig.expected <- Egig(lambda, chi, psi, func = "x")
  q.expected <- pgig(gig.expected, lambda, chi, psi,
                     subdivisions = subdivisions, 
                     rel.tol = rel.tol, abs.tol= abs.tol)
  if(q.expected > max(p)){
    interval <- c(0, gig.expected)
  }else{
    interval.max <- internal.bisection(lambda, chi, psi, max(p),
                                       root.tol, rel.tol, abs.tol, subdivisions)
    interval <- c(0, interval.max * 1.1)                                
  }
  if(any(is.na(interval))){ # -> Failed to determine bounds for the quantiles
    p.raw[is.finite(p.raw)] <- NA
    return(p.raw)
  }
      
  if(method=="integration"){
    ## The integration method 
    pdf.args <- list(lambda = lambda, chi = chi, psi = psi)
    p <- matrix(p, ncol = 1)
    value <- apply(p, MARGIN = 1, FUN = q.default, pdf = "dgig",
                   pdf.args = pdf.args, interval = interval, 
                   tol = root.tol, p.lower = 0, rel.tol = rel.tol, 
                   abs.tol = abs.tol, subdivisions = subdivisions)
  }else{
    ## The spline method 
    interval.seq <- seq(min(interval), max(interval), length = spline.points)
    ## Compute the distribution function to be interpolated by splines
    p.interval <- pgig(q = interval.seq, lambda = lambda, chi = chi, 
                       psi = psi, rel.tol = rel.tol, abs.tol = abs.tol, 
                       subdivisions = subdivisions)
    ## Spline function
    spline.distribution.func <- splinefun(interval.seq, p.interval)

    ## root function:   condition: quantile.root.func == 0
    quantile.root.func <- function(x, tmp.p){
      spline.distribution.func(x) - tmp.p   
    }
    value <- p
    for(i in 1:length(p)){
      value[i] <- uniroot(quantile.root.func, interval = interval,
                          tmp.p = p[i], tol = root.tol)$root
    }
    value <- as.vector(value)
  }
  p.raw[is.finite(p.raw)] <- value
  return(p.raw)
}
