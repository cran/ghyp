"qghyp" <- function(p, object = ghyp(), method = c("integration", "splines"),
                    spline.points = 200, subdivisions = 200, 
                    root.tol = .Machine$double.eps^0.5,
                    rel.tol = root.tol^1.5, abs.tol = rel.tol)

{
  ## Only univariate 'ghyp' objects are allowed
  test.ghyp(object, case = "univariate")

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
  internal.newton <- function(object, p, tol, rel.tol, abs.tol, subdivisions)
  {
    newton.tol <- 1
    iter <- 0
    x0 <- mean(object)
    while(newton.tol > tol & iter < 100){
      iter <- iter + 1
      F1 <- pghyp(q = x0, object, rel.tol = rel.tol, abs.tol = abs.tol,
                  subdivisions = subdivisions) - p
      x1 <- x0 - F1/dghyp(x0, object)
      newton.tol <- abs((x1 - x0)/x0)
      x0 <- x1
##      cat("x0: ",x0, "  F1: ", F1, " newton.tol", newton.tol,"\n")
      if(is.nan(newton.tol)){
        stop("Unable to determine interval where the quantiles are in-between.\n",
              "Perhaps the skewness is too large!") 
      }
    }
    return(x0)
  }
  #<---------------- end of Newton iteration  ---------------------->

  if(length(p) == 1){
    ## If a single quantile is requested use the newton method anyway
    value <- internal.newton(object, p, root.tol, rel.tol, abs.tol, subdivisions)
    p.raw[is.finite(p.raw)] <- value
    return(p.raw)
  }else if(length(p) == 2){
    ## If two quantiles are requested use the newton method anyway
    value1 <- internal.newton(object, p[1], root.tol, rel.tol, abs.tol, subdivisions)
    value2 <- internal.newton(object, p[2], root.tol, rel.tol, abs.tol, subdivisions)
    p.raw[is.finite(p.raw)] <- c(value1, value2)
    return(p.raw)
  }else{
    ## If more than two quantiles are requested use the newton method 
    ## to find the range where the quantiles can be found.
    q.min <- internal.newton(object, min(p), root.tol, rel.tol, abs.tol, subdivisions)
    q.max <- internal.newton(object, max(p), root.tol, rel.tol, abs.tol, subdivisions)

    interval <- c(q.min, q.max)

    ## Extend the interval by 10 percent so that 'uniroot' does not crash
    interval <- c(interval[1] - 0.01 * diff(range(interval)),
                  interval[2] + 0.01 * diff(range(interval)))
    #<---------------------  Integration method  ------------------------->
    if(method == "integration"){
      pdf.args <- list(lambda = object@lambda, chi = object@chi, psi = object@psi,
                       mu = object@mu, sigma = object@sigma, gamma = object@gamma)
      p <- matrix(p, ncol = 1)
      q.val <- apply(p, MARGIN = 1, FUN = q.default, pdf = "internal.dghyp",
                     pdf.args = pdf.args, interval = interval, tol = root.tol,
                     p.lower = -Inf, rel.tol = rel.tol, abs.tol = abs.tol,
                     subdivisions = subdivisions)
      value <- sapply(q.val, FUN = function(x)x$root)
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
