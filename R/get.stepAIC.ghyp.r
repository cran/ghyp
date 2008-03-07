"get.stepAIC.ghyp" <- function(stepAIC.obj, dist = c("ghyp", "hyp", "NIG", "VG", "t", "gauss"), 
                               symmetric = FALSE)
{
 dist <- match.arg(dist, several.ok = FALSE)
 
 if(is.null(symmetric) || is.na(symmetric)){
   stop("'symmetric' must be either 'TRUE' or 'FALSE'!")
 } 
 
 if(dist == "gauss" && !symmetric){
   stop("Asymmetric 'gauss' does not exist!")
 }
 
 table.idx <- which(stepAIC.obj$fit.table$model == dist & 
                    stepAIC.obj$fit.table$symmetric == symmetric)
 
 if(length(table.idx) == 0){
   stop("Model '", dist, "' for symmetric = ", symmetric, " not found!")
 }
 
 model.idx <- as.numeric(rownames(stepAIC.obj$fit.table)[table.idx])
 
 return(list(model = stepAIC.obj$all.models[[model.idx]], 
             fit.info = stepAIC.obj$fit.table[table.idx, ]))
}
