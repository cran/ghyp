test.class.ghyp <- function(object,case=c("ghypbase","ghypuv","ghypmv")){
  case = match.arg(case)
##  print(case)
##  print(object)
##  print(class(object))
  if(case=="ghypbase"){
    if(!is(object, "ghypbase")){
      stop("Object does not inherit from class 'ghypbase'!\n")
    }
  }else if(case=="ghypuv"){
    if(!is(object, "ghypuv")){
      stop("Object is not of class 'ghypuv' nor of class 'mle.ghypuv'!\n")
    }
  
  }else if(case=="ghypmv"){
    if(!is(object, "ghypmv")){
      stop("Object is not of class 'ghypmv' nor of class 'mle.ghypmv'!\n")
    }
  }
}
