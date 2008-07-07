"qqghyp" <- function(object, data = ghyp.data(object),
                     gaussian = TRUE, line = TRUE,
                     main = "Generalized Hyperbolic Q-Q Plot",
                     xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
                     ghyp.pch = 1, gauss.pch = 6, ghyp.lty = "solid",
                     gauss.lty = "dashed", ghyp.col = "black",
                     gauss.col = "black",
                     plot.legend = TRUE, location = "topleft", legend.cex = 0.8,
                     spline.points = 150, root.tol = .Machine$double.eps^0.5,
                     rel.tol = root.tol, abs.tol = root.tol^1.5, add = FALSE, ...)
{
  test.ghyp(object, case = "univariate")
  
  data <- check.data(data, case = "uv", na.rm = T, fit = TRUE, dim = 1)
  
  ## compute quantiles   
  ghyp.q <- qghyp(ppoints(length(data)), object, method = "splines",
                  spline.points = spline.points, root.tol = root.tol,
                  rel.tol = rel.tol, abs.tol = abs.tol)[order(order(data))]

  ## plot ghyp quantiles
  if(add){
    points(ghyp.q, data, pch = ghyp.pch, col = ghyp.col, ...)
  }else{
    plot(ghyp.q, data, xlab = xlab, ylab = ylab, pch = ghyp.pch, 
         col = ghyp.col, main = main, ...)
  }

  if(gaussian){
    gauss.q <- qnorm(ppoints(length(data)), mean = mean(data), sd = sd(data))[order(order(data))]
    points(gauss.q, data, pch = gauss.pch, col = gauss.col)
  }
  
  if(line){
    abline(lm(data ~ ghyp.q), lty = ghyp.lty, col = ghyp.col)
    if(gaussian){
      abline(lm(data ~ gauss.q), lty = gauss.lty, col = gauss.col)
    }
  }

 if(plot.legend && !add){
  if(gaussian){
    legend(location, legend = c(ghyp.name(object, abbr = TRUE, skew.attr = TRUE), "Gaussian"), 
           col = c(ghyp.col, gauss.col), lty = c(ghyp.lty, gauss.lty), 
           cex = legend.cex, pch = c(ghyp.pch, gauss.pch))
  }else{
    legend(location, legend = ghyp.name(object, abbr = TRUE, skew.attr = TRUE), 
           col = ghyp.col, lty = ghyp.lty, cex = legend.cex, pch = ghyp.pch)
  }

 }
}
