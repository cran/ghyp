"ghyp.name" <- function(object, abbr = FALSE, skew.attr = TRUE){
    test.ghyp(object, case = "ghyp")
    if(!abbr && skew.attr){
      return(object@model[1])
    }else if(abbr && skew.attr){
      return(object@model[3])
    }else if(abbr && !skew.attr){
      return(object@model[4])
    }else{
      return(object@model[2])
    }        
}
