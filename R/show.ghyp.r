"show.ghyp" <- function(object)
{
  cat(ghyp.name(object, abbr = FALSE, skew.attr = TRUE), "Distribution:\n", sep = " ")
  cat("\nParameters:\n")
  if(object@dimension > 1){ # Multivariate case
    param <- coef(object)  
    if(object@parametrization == "alpha.delta"){
      if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "t"){                   
        # Student-t  ->  alpha^2 == beta' Delta beta
        param.uv <- unlist(param[c(1, 3)])       
      }else if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "VG"){
        # VG  ->  delta == 0 
        param.uv <- unlist(param[1:2])
      }else if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "ghyp"){
        param.uv <- unlist(param[1:3])
      }else{
        # hyp or NIG -> lambda is constant
        param.uv <- unlist(param[2:3])  
      }
      print(param.uv)
      cat("\nmu:\n")
      print(param$mu)
      cat("\nDelta:\n")
      print(param$Delta)
      cat("\nbeta:\n")
      print(param$beta)  
    }else if(is.gaussian(object)){
      cat("\nmu:\n")
      print(param$mu)
      cat("\nsigma:\n")
      print(param$sigma)
    }else{
      if(object@parametrization == "chi.psi"){
        param.uv <- unlist(param[1:3])
      }else{
        param.uv <- unlist(param[1:2])
      }
      if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "t"){                   
        # Student-t  ->  alpha.bar == 0
        if(object@parametrization == "chi.psi"){
          param.uv <- param.uv[c("lambda", "chi")]
        }else{
          param.uv <- param.uv["nu"]
        }
      }else if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "VG"){
        # VG  ->  alpha.bar == 0 or chi == 0
        if(object@parametrization == "chi.psi"){
          param.uv <- param.uv[c(1, 3)]
        }else if(object@parametrization == "alpha.bar"){
          param.uv <- param.uv[1]
        }
      }else if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "ghyp"){
        #param.uv <- param.uv
      }else{
        # hyp or NIG -> lambda is constant
        param.uv <- param.uv[-1]    
      }
      print(param.uv)
      cat("\nmu:\n")
      print(param$mu)
      cat("\nsigma:\n")
      print(param$sigma)
      cat("\ngamma:\n")
      print(param$gamma)  
    }
  }else{  # Univariate case
    param <- unlist(coef(object))
    if(object@parametrization == "alpha.delta"){ #  ----> alpha.delta-parametrization
      if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "t"){
        # Student-t  ->  alpha^2 == beta^2
        param <- param[-2]
      }else if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "VG"){
        # VG  ->  delta == 0
        param <- param[-5]
      }else if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "ghyp"){
        # param <- param
      }else{
        # hyp or NIG -> lambda is constant
        param <- param[-1]    
      }
    }else if(is.gaussian(object)){
      param <- c(param["mu"], param["sigma"])
    }else{ #  ----> chi.psi or alpha.bar-parametrization
      if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "t"){
        # Student-t  ->  alpha.bar == 0
        if(object@parametrization == "chi.psi"){
          param <- param[-3]
        }else{
          param <- param[-c(1, 3)]
        }
      }else if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "VG"){
        # VG  ->  alpha.bar == 0 or chi == 0
        param <- param[-2]
      }else if(ghyp.name(object, abbr = TRUE, skew.attr = FALSE) == "ghyp"){
        # param <- param
      }else{
        # hyp or NIG -> lambda is constant
        param <- param[-1]    
      }
    }
    print(param)
  }

}

setMethod("show", signature(object = "ghyp"), show.ghyp)
