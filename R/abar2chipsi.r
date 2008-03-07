"abar2chipsi" <- function(alpha.bar, lambda, eps = .Machine$double.eps)
{
    if (alpha.bar < 0) {
      stop("alpha.bar must be non-negative.") 
    }
    if (alpha.bar > eps) {
      if (lambda >= 0) {
        psi = alpha.bar * besselK(alpha.bar, lambda + 1, expon.scaled = TRUE)/
          besselK(alpha.bar, lambda, expon.scaled = TRUE)
        if (is.na(psi)) {
          psi <- 200
          #warning( paste( "Overflow in besselK, alpha.bar = ",
          #               alpha.bar, "; lambda = ", lambda) )
        }
        chi <- alpha.bar^2 / psi
      } else {
        chi <- alpha.bar * besselK(alpha.bar, lambda, expon.scaled = TRUE)/
          besselK(alpha.bar, lambda + 1, expon.scaled = TRUE)
        if (is.na(chi) ) {
          chi <- 200
          #warning( paste( "Overflow in besselK, alpha.bar = ",
          #               alpha.bar, "; lambda = ", lambda) )
        }
        psi <- alpha.bar^2 / chi
      }
    } else {
        if (lambda > 0) { # VG
            chi <- 0
            psi <- 2 * lambda
        } else if (lambda < 0) { # Student-t
            psi <- 0
            chi <- 2 * (-lambda - 1)
         } else {
            stop("Forbidden combination of parameter values");
        }
    }
    list("lambda" = unname(lambda), "chi" = unname(chi), "psi" = unname(psi))
}

