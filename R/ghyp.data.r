"ghyp.data" <- function(object){
    test.class.ghyp(object,case="ghypbase")
    if(length(object@data)!=0){
      return(object@data)
    }else{
      return(NULL)
    }
}