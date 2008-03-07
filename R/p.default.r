"p.default" <- function(q, pdf, pdf.args, lower, upper, ...){
  if(missing(upper)){
    int.pars <- list(f = pdf, lower = lower, upper = q)
  }else{
    int.pars <- list(f = pdf, lower = q, upper = upper)  
  }
  tmp.prob <- try(do.call("integrate", c(pdf.args, int.pars, list(...))))
  if(class(tmp.prob) == "try-error"){
     warning("Failed to determine probability with 'q = ", q,
             "'!\nMessage: ", as.character(tmp.prob), "\n")
     return(NA)
  }else{
     return(tmp.prob$value)   
  }   
}
