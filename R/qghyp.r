"qghyp" <- function(p, object = ghyp(), method = c("integration", "splines"),
                    spline.points = 200, subdivisions = 200, 
                    root.tol = .Machine$double.eps^0.5,
                    rel.tol = root.tol^1.5, abs.tol = rel.tol)

{
  ## Only univariate 'ghyp' objects are allowed
  test.ghyp(object, case = "univariate")

  if(is.gaussian(object)){
    return(qnorm(p, mean = object@mu, sd = object@sigma))
  }else if(is.student.t(object, symmetric = TRUE)){
     nu <- coef(object)$nu
     return(qt(p, df = nu) * sqrt((nu - 2) / nu) * object@sigma + object@mu)  
  }

  p.raw  <- check.data(p, na.rm = FALSE, fit = FALSE, dim = 1)

  method <- match.arg(method)

  p.raw[p.raw < 0 | p.raw > 1] <- NaN
  p.raw[p.raw == 1] <- Inf
  p.raw[p.raw == 0] <- -Inf

  ## If only non-finite quantiles are passed return NA, NaN, Inf, -Inf
  p <- p.raw[is.finite(p.raw)]
  if(length(p) == 0){
    return(p.raw)
  }
  #<----   Use Newton's method to find the range of the quantiles ---->

  internal.bisection <- function(object, p, tol, rel.tol, abs.tol, subdivisions)
  {
    iter <- 0
    range.found <- FALSE
    step.size <- sqrt(vcov(object))

    if(!is.finite(step.size)){
      step.size <- coef(object, type = "chi.psi")$sigma / 2
    }

    q.0 <- mean(object)
    q.range <- c(q.0 - step.size, q.0 + step.size)

    while(!range.found & iter < 100){
      iter <- iter + 1
      p.range <- pghyp(q = q.range, object, rel.tol = rel.tol, abs.tol = abs.tol,
                       subdivisions = subdivisions) - p  
      
      if(any(is.na(p.range))){
        warning("Unable to determine interval where the quantiles are in-between.\n",
                "Perhaps the skewness is too large!") 
        return(NA)              
      }

      lower <- p.range[1]
      upper <- p.range[2]
       
##      cat("lower: ", lower,";  upper : ", upper, "\n")
      if(upper < 0 & lower < 0){
        q.range[1] <- q.range[2]
        q.range[2] <- q.range[2] + step.size
        next
      }
      if(upper > 0 & lower > 0){
        q.range[2] <- q.range[1]
        q.range[1] <- q.range[1] - step.size
        next
      }
      if(upper > 0 & lower < 0){
        range.found <- TRUE
      }
    }
    if(iter >= 100){
      warning("Unable to determine interval where the quantiles are in-between.\n",
              "Perhaps the skewness is too large!") 
    }
    
    q.root <- q.default(p, pdf = "internal.dghyp", pdf.args = coef(object, type = "chi.psi"), 
                        interval = q.range, tol = root.tol,
                        p.lower = -Inf, rel.tol = rel.tol, abs.tol = abs.tol,
                        subdivisions = subdivisions)
    return(q.root)
  }
 
  
  #<---------------- end of Newton iteration  ---------------------->

  if(length(p) == 1){
    ## If a single quantile is requested use the newton method anyway
    value <- internal.bisection(object, p, root.tol, rel.tol, abs.tol, subdivisions)
    p.raw[is.finite(p.raw)] <- as.numeric(value)
    return(p.raw)
  }else if(length(p) == 2){
    ## If two quantiles are requested use the newton method anyway
    value1 <- internal.bisection(object, p[1], root.tol, rel.tol, abs.tol, subdivisions)
    value2 <- internal.bisection(object, p[2], root.tol, rel.tol, abs.tol, subdivisions)
    p.raw[is.finite(p.raw)] <- c(value1, value2)
    return(p.raw)
  }else{
    ## If more than two quantiles are requested use the newton method 
    ## to find the range where the quantiles can be found.
    q.min <- internal.bisection(object, min(p), root.tol, rel.tol, abs.tol, subdivisions)
    q.max <- internal.bisection(object, max(p), root.tol, rel.tol, abs.tol, subdivisions)

    interval <- c(q.min, q.max)
    
    if(any(is.na(interval))){ # -> Failed to determine bounds for the quantiles
      p.raw[is.finite(p.raw)] <- NA
      return(p.raw)
    }

    ## Extend the interval by 10 percent so that 'uniroot' does not crash
    interval <- c(interval[1] - 0.01 * diff(range(interval)),
                  interval[2] + 0.01 * diff(range(interval)))
    #<---------------------  Integration method  ------------------------->
    if(method == "integration"){
      pdf.args <- coef(object, type = "chi.psi")
      p <- matrix(p, ncol = 1)
      value <- apply(p, MARGIN = 1, FUN = q.default, pdf = "internal.dghyp",
                     pdf.args = pdf.args, interval = interval, tol = root.tol,
                     p.lower = -Inf, rel.tol = rel.tol, abs.tol = abs.tol,
                     subdivisions = subdivisions)
    }else{
    #<-----------------------  Splines method  --------------------------->
      interval.seq <- seq(min(interval), max(interval), length = spline.points)
      ## Compute the distribution function to be interpolated by splines
      p.interval <- pghyp(q = interval.seq, object, rel.tol = rel.tol, 
                          abs.tol = abs.tol, subdivisions = subdivisions)

      ## Spline function
      spline.distribution.func <- splinefun(interval.seq, p.interval)

      ## root function:   condition: quantile.root.func == 0
      quantile.root.func <- function(x, tmp.p){
        spline.distribution.func(x) - tmp.p   
      }

      value <- p

      for(i in 1:length(p)){
        value[i] <- uniroot(quantile.root.func, interval = interval,
                            tol = root.tol, tmp.p = p[i])$root
      }
    }
    p.raw[is.finite(p.raw)] <- value
    return(p.raw)
  }
}
