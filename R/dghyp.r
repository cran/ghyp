"dghyp" <- function(x,object=ghyp(),logvalue=F)
{
  test.class.ghyp(object,case="ghypbase")
  x <- check.data(x,case = if(object@dimension>1)"mv" else "uv",
                  na.rm=FALSE,fit=FALSE,dim=object@dimension)
  if(is(object, "ghypuv")){
    return(unname(internal.dghyp(x, lambda= object@lambda, chi=object@chi, psi=object@psi,
                          mu=object@mu, sigma=object@sigma , gamma=object@gamma ,
                          logvalue=logvalue)))
  }else{
    return(unname(internal.dghypmv(x, object@lambda, object@chi, object@psi,
                            object@mu, object@sigma , object@gamma ,logvalue)))
  }
}

