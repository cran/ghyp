"check.opt.pars" <- function(opt.pars){
  default.names <- c("lambda","alpha.bar","mu","sigma","gamma")
  ## There are 3 possibilities:
  ## (1) opt.pars are not named: They must be of length 5
  ## (2) opt.pars are named and are in 'default.names': 
  ##     (*) Any unknown names are dropped 
  ##     (*) opt.pars is reordered
  ## (3) If unknown opt.pars are passed an error occurs
  if(is.null(names(opt.pars))){
    if(length(opt.pars)==5){
      new.opt.pars <- c(lambda=opt.pars[1],alpha.bar=opt.pars[2],mu=opt.pars[3],
                        sigma=opt.pars[4],gamma=opt.pars[5])
    }else{
      stop("If opt.pars is not named it must have length 5!\n",
      "The order is lambda, alpha.bar, mu, sigma, gamma.\n")
    }
  }else{
    if(all(default.names %in% names(opt.pars))){
      if(length(opt.pars)!=5){
        warning("The following names were dropped:\n",
        paste(names(opt.pars)[!(names(opt.pars) %in% default.names)],collapse=", "))
      }
      new.opt.pars <- c(lambda=unname(opt.pars["lambda"]),
                        alpha.bar=unname(opt.pars["alpha.bar"]),
                        mu=unname(opt.pars["mu"]),
                        sigma=unname(opt.pars["sigma"]),
                        gamma=unname(opt.pars["gamma"]))
      }else if(!any(default.names %in% names(opt.pars))){
        stop("The names '", paste(names(opt.pars),collapse="', '"),
        "' do not match the required names lambda, alpha.bar, mu, sigma, gamma.\n")
      }else if(!all(names(opt.pars) %in% default.names)){
        stop("The names '", paste(names(opt.pars)[!(names(opt.pars) %in% default.names)],collapse="', '"),
        "' do not match the required names lambda, alpha.bar, mu, sigma, gamma.\n")
      }
      else{
        new.opt.pars <- NULL
        if("lambda" %in% names(opt.pars)){
          new.opt.pars <-  c(new.opt.pars,lambda=unname(opt.pars["lambda"]))
        }else{
          new.opt.pars <-  c(new.opt.pars,lambda=TRUE)
        }
        if("alpha.bar" %in% names(opt.pars)){
          new.opt.pars <-  c(new.opt.pars,alpha.bar=unname(opt.pars["alpha.bar"]))
        }else{
          new.opt.pars <-  c(new.opt.pars,alpha.bar=TRUE)
        }
        if("mu" %in% names(opt.pars)){
          new.opt.pars <-  c(new.opt.pars,mu=unname(opt.pars["mu"]))
        }else{
          new.opt.pars <-  c(new.opt.pars,mu=TRUE)
        }
        if("sigma" %in% names(opt.pars)){
          new.opt.pars <-  c(new.opt.pars,sigma=unname(opt.pars["sigma"]))
        }else{
          new.opt.pars <-  c(new.opt.pars,sigma=TRUE)
        }
        if("gamma" %in% names(opt.pars)){
          new.opt.pars <-  c(new.opt.pars,gamma=unname(opt.pars["gamma"]))
        }else{
          new.opt.pars <-  c(new.opt.pars,gamma=TRUE)
        }
    }
  }
  return(new.opt.pars)
}
