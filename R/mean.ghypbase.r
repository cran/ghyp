"mean.ghypbase" <- function(x){
return(x@expected.value)
}
setMethod("mean", signature(x="ghypbase"),mean.ghypbase)
