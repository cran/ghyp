test.ghyp <- function(object, case = c("ghyp", "univariate", "multivariate")){
  case = match.arg(case)

  if(!is(object, "ghyp")){
    stop("Object must be of class 'ghyp'!\n")
  }

  if(case == "univariate"){
    if(!is.univariate(object)){
      stop("'ghyp' object must be univariate (i.e. dimension == 1)!\n")
    }
  }else if(case == "multivariate"){
    if(object@dimension <= 1){
      stop("'ghyp' object must be multivariate (i.e. dimension > 1)!\n")
    }
  }
}
