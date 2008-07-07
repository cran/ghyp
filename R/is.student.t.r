"is.student.t" <- function(object, symmetric = NULL)
{
  if(is.null(symmetric)){
    return(ghyp.name(object, abbr = FALSE, skew.attr = FALSE) == "Student-t")  
  }else if(symmetric){
    return(ghyp.name(object, abbr = FALSE, skew.attr = TRUE) == "Symmetric Student-t")
  }else if(!symmetric){
    return(ghyp.name(object, abbr = FALSE, skew.attr = TRUE) == "Asymmetric Student-t") 
  }else{
    stop("Undefined value received in 'is.student.t'!\n")
  }
}
