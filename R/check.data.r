"check.data" <- function(data, case=c("uv", "mv"), na.rm = TRUE, 
                         fit = TRUE, dim = NULL)
{
  case <- match.arg(case)
  #<------------------ MULTIVARIATE -------------------->
  if(case=="mv"){
    data <- as.matrix(data)
    if(ncol(data)<2){
      stop("'data' must have more than one column. Try the univariate case!")
    }
    
    na.idx <- apply(is.na(data),1,any)
    if(na.rm){
      if(any(na.idx)){
        if(all(na.idx)){
          stop("Sample contains only columns with NA's!")
        }else{
          warning(sum(na.idx)," NA observations removed")
        }
        data <- data[!na.idx,]
      }
    }else{
      if(all(na.idx)){
        stop("Sample contains only columns with NA's!")
      }
    }
    if(nrow(data) < ncol(data) & fit){
      stop("'data' must not have more columns than rows.")
    }
  }else{
  #<------------------ UNIVARIATE -------------------->
    if(!is.vector(data)){
      data <- as.matrix(data)
      if(ncol(data)>1){
        stop("'data' must have only one column!")
      }
      data <- as.vector(data)
    }
    
    na.idx <- is.na(data)
    if(na.rm){
      if(any(na.idx)){
        if(all(na.idx)){
          stop("Sample contains only NA's!")
        }else{
          warning(sum(na.idx)," NA observations removed")
        }
      }
      data <- data[!na.idx]
    }else{
      if(all(na.idx)){
        stop("Sample contains only NA's!")
      }
    }

    if(fit & length(data) < 5){
      stop("Length of 'data' must be at least 5.")
    }
  }
  if(!is.numeric(data)){
    stop("'data' must be numeric!")
  }
  return(data)
}


