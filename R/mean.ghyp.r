"mean.ghyp" <- function(x)
{
  return(x@expected.value)
}
setMethod("mean", signature(x = "ghyp"), mean.ghyp)
