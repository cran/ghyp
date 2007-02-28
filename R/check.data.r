"check.data" <- function(data,case=c("uv","mv"),na.rm=T,fit=TRUE,dim=NULL){
  case <- match.arg(case)
  #<------------------ MULTIVARIATE -------------------->
  if(case=="mv"){
    if(is.data.frame(data)){
      if(any(!sapply(data,is.numeric))){
        stop("All columns must be of type 'numeric'! ",
             "Perhaps there is a columns with only NA's.")
      }
      if(fit & ncol(data)>nrow(data)){
        stop("'data' must not have more columns than rows.")
      }
      data <- as.matrix(data)
    }else if(is.matrix(data)){
      if(ncol(data)>nrow(data)){
        if(fit){
          warning("'data' has more columns than rows and will be transposed!")
          data <- t(data)
        }
      }
    }else{
      if(fit){
        stop("'data' must be of class 'matrix' or 'data.frame'!")
      }
      data <- matrix(data,nrow=1)
    }
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
    if(is.data.frame(data)){
      if(ncol(data)>1){
        stop("data.frame 'data' must have only one column!")
      }
      if(any(!sapply(data,is.numeric))){
        stop("Column must be of type 'numeric'! Perhaps all observations are NA.")
      }
      data <- as.matrix(data)
    }else if(is.matrix(data)){
      if(ncol(data)>1){
        stop("data.frame 'data' must have only one column!")
      }
      data <- as.vector(data)
    }else{
      if(!is.numeric(data)){
        stop("'data' must be of class 'numeric', 'matrix' or 'data.frame'!")
      }
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
  if(!is.null(dim)){
    if(dim > 1){
      if(dim != ncol(data)){
        stop("Dimension missmatch!")
      }
    }
  }
  return(data)
}


