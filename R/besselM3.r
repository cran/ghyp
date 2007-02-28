"besselM3" <- function(lambda = 9/2, x = 2, logvalue = FALSE)
{
  if (logvalue == FALSE)
    res = besselK(x, lambda)
  else
    res = log( besselK(x, lambda, expon.scaled = TRUE) ) - x
  res
}