"fit.ghyp" <- function(object, llh = 0, n.iter = 0, converged = FALSE, error.code = 0,
                       error.message = "", parameter.variance, fitted.params, aic, 
                       trace.pars = list())                                         
{
  if(missing(parameter.variance)){
    parameter.variance <- matrix(0)
  }

  return(new("mle.ghyp", call = object@call, lambda = object@lambda, chi = object@chi, 
             psi = object@psi, alpha.bar = object@alpha.bar, mu = object@mu,
             sigma = object@sigma, gamma = object@gamma, model = object@model,
             dimension = object@dimension, expected.value = object@expected.value,
             variance = object@variance, data = object@data, 
             parametrization = object@parametrization, llh =llh,
             n.iter = n.iter, converged = converged, error.code = error.code,
             error.message = error.message, 
             parameter.variance = parameter.variance,
             fitted.params = fitted.params, aic = aic, 
             trace.pars = trace.pars))
}
