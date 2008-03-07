"ghyp.data" <- function(object){
    test.ghyp(object, case = "ghyp")
    if(length(object@data) != 0){
      if(is.univariate(object)){
        return(as.vector(object@data))      
      }else{
        return(object@data)
      }
    }else{
      return(NULL)
    }
}
