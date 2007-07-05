"p.default" <- function(q,pdf,pdf.args,lower,...){
  int.pars <- list(f=pdf,lower=lower,upper=q)
  do.call("integrate",c(pdf.args,int.pars,list(...)))
}