"plot.ghyp" <- function(x, range = qghyp(c(0.001, 0.999), x), 
                          length = 1000, ...)
{
  test.ghyp(x, "univariate")
  if(length(range) > 2){
    x.seq <- range
  }else{
    x.seq <- seq(min(range), max(range), length = length)
  }
  ghyp.density <- dghyp(x.seq, x)
  plot(x.seq, ghyp.density, ...)
}

setMethod("plot", signature(x = "ghyp",y = "missing"), plot.ghyp)

