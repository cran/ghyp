"[.ghyp" <- function(x, i = c(1, 2))
{
  test.ghyp(x, case = "multivariate")
  i <- as.integer(i)
  if(min(i) >= 1 & max(i) <= x@dimension){
    if(length(x@data) == 0){
      data <- NULL
    }else{
      data <- x@data[, i]
    }
    if(length(i)==1){
      return(ghyp(lambda = x@lambda, chi = x@chi, psi = x@psi,
                  mu = x@mu[i], sigma = sqrt(x@sigma[i, i]),
                  gamma = x@gamma[i], data = data))
    }else{
      return(ghyp(lambda = x@lambda, chi = x@chi, psi = x@psi,
                  mu = x@mu[i], sigma = x@sigma[i, i],
                  gamma = x@gamma[i], data = data))
    }
  }else{
    stop("Dimension mismatch. ghyp dimension = ", x@dimension,
         "; Input = ",paste(i, collapse=", "), "!\n", sep="")
  }
}
setMethod("[", signature(x = "ghyp", i = "numeric", j = "missing", drop = "missing"), `[.ghyp`)

"redim" <- function(x, i = c(1, 2))
{
  warning("'redim' is replaced by '[' and will be removed in the next release!") 
  return(x[i])

}
