pkgname <- "ghyp"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ghyp')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("coef-method")
### * coef-method

flush(stderr()); flush(stdout())

### Name: coef-method
### Title: Extract parameters of generalized hyperbolic distribution
###   objects
### Aliases: coef.ghyp coef,ghyp-method coefficients,ghyp-method
### Keywords: methods utilities

### ** Examples

  ghyp.mv <- ghyp(lambda = 1, alpha.bar = 0.1, mu = rep(0,2), sigma = diag(rep(1,2)),
                  gamma = rep(0,2), data = matrix(rt(1000, df = 4), ncol = 2))
  ## Get parameters
  coef(ghyp.mv, type = "alpha.bar")
  coefficients(ghyp.mv, type = "chi.psi")

  ## Simple modification (do not modify slots directly e.g. object@mu <- 0:1)
  param <- coef(ghyp.mv, type = "alpha.bar")
  param$mu <- 0:1
  do.call("ghyp", param) # returns a new 'ghyp' object





cleanEx()
nameEx("fit.ghypmv")
### * fit.ghypmv

flush(stderr()); flush(stdout())

### Name: fit.ghypmv
### Title: Fitting generalized hyperbolic distributions to multivariate
###   data
### Aliases: fit.ghypmv fit.hypmv fit.NIGmv fit.VGmv fit.tmv fit.gaussmv
### Keywords: iteration optimize distribution models multivariate

### ** Examples

  data(smi.stocks)

  fit.ghypmv(data = smi.stocks, opt.pars = c(lambda = FALSE), lambda = 2,
             control = list(rel.tol = 1e-5, abs.tol = 1e-5), reltol = 0.01)



cleanEx()
nameEx("fit.ghypuv")
### * fit.ghypuv

flush(stderr()); flush(stdout())

### Name: fit.ghypuv
### Title: Fitting generalized hyperbolic distributions to univariate data
### Aliases: fit.ghypuv fit.hypuv fit.NIGuv fit.VGuv fit.tuv fit.gaussuv
### Keywords: iteration optimize distribution models

### ** Examples

  data(smi.stocks)

  nig.fit <- fit.NIGuv(smi.stocks[,"SMI"], opt.pars = c(alpha.bar = FALSE),
                       alpha.bar = 1, control = list(abs.tol = 1e-8))
  nig.fit

  summary(nig.fit)

  hist(nig.fit)



cleanEx()
nameEx("ghyp-constructors")
### * ghyp-constructors

flush(stderr()); flush(stdout())

### Name: ghyp-constructors
### Title: Create generalized hyperbolic distribution objects
### Aliases: ghyp hyp NIG student.t VG gauss ghyp.ad hyp.ad NIG.ad
###   student.t.ad VG.ad
### Keywords: distribution multivariate models

### ** Examples

  ## alpha.bar parametrization of a univariate GH distribution
  ghyp(lambda=2, alpha.bar=0.1, mu=0, sigma=1, gamma=0)
  ## lambda/chi parametrization of a univariate GH distribution
  ghyp(lambda=2, chi=1, psi=0.5, mu=0, sigma=1, gamma=0)
  ## alpha/delta parametrization of a univariate GH distribution
  ghyp.ad(lambda=2, alpha=0.5, delta=1, mu=0, beta=0)

  ## alpha.bar parametrization of a multivariate GH distribution
  ghyp(lambda=1, alpha.bar=0.1, mu=2:3, sigma=diag(1:2), gamma=0:1)
  ## lambda/chi parametrization of a multivariate GH distribution
  ghyp(lambda=1, chi=1, psi=0.5, mu=2:3, sigma=diag(1:2), gamma=0:1)
  ## alpha/delta parametrization of a multivariate GH distribution
  ghyp.ad(lambda=1, alpha=2.5, delta=1, mu=2:3, Delta=diag(c(1,1)), beta=0:1)

  ## alpha.bar parametrization of a univariate hyperbolic distribution
  hyp(alpha.bar=0.3, mu=1, sigma=0.1, gamma=0)
  ## lambda/chi parametrization of a univariate hyperbolic distribution
  hyp(chi=1, psi=2, mu=1, sigma=0.1, gamma=0)
  ## alpha/delta parametrization of a univariate hyperbolic distribution
  hyp.ad(alpha=0.5, delta=1, mu=0, beta=0)

  ## alpha.bar parametrization of a univariate NIG distribution
  NIG(alpha.bar=0.3, mu=1, sigma=0.1, gamma=0)
  ## lambda/chi parametrization of a univariate NIG distribution
  NIG(chi=1, psi=2, mu=1, sigma=0.1, gamma=0)
  ## alpha/delta parametrization of a univariate NIG distribution
  NIG.ad(alpha=0.5, delta=1, mu=0, beta=0)

  ## alpha.bar parametrization of a univariate VG distribution
  VG(lambda=2, mu=1, sigma=0.1, gamma=0)
  ## alpha/delta parametrization of a univariate VG distribution
  VG.ad(lambda=2, alpha=0.5, mu=0, beta=0)

  ## alpha.bar parametrization of a univariate t distribution
  student.t(nu = 3, mu=1, sigma=0.1, gamma=0)
  ## alpha/delta parametrization of a univariate t distribution
  student.t.ad(lambda=-2, delta=1, mu=0, beta=1)

  ## Obtain equal results as with the R-core parametrization
  ## of the t distribution:
  nu <- 4
  standard.R.chi.psi <- student.t(nu = nu, chi = nu)
  standard.R.alpha.bar <- student.t(nu = nu, sigma = sqrt(nu  /(nu - 2)))

  random.sample <- rnorm(3)
  dt(random.sample, nu)
  dghyp(random.sample, standard.R.chi.psi)   # all implementations yield...
  dghyp(random.sample, standard.R.alpha.bar) # ...the same values

  random.quantiles <- runif(4)
  qt(random.quantiles, nu)
  qghyp(random.quantiles, standard.R.chi.psi)   # all implementations yield...
  qghyp(random.quantiles, standard.R.alpha.bar) # ...the same values

  ## If nu <= 2 the "alpha.bar" parametrization does not exist, but the
  ## "chi/psi" parametrization. The case of a Cauchy distribution:
  nu <- 1
  standard.R.chi.psi <- student.t(nu = nu, chi = nu)

  dt(random.sample, nu)
  dghyp(random.sample, standard.R.chi.psi)   # both give the same result

  pt(random.sample, nu)
  pghyp(random.sample, standard.R.chi.psi) # both give the same result




cleanEx()
nameEx("ghyp-distribution")
### * ghyp-distribution

flush(stderr()); flush(stdout())

### Name: ghyp-distribution
### Title: The Generalized Hyperbolic Distribution
### Aliases: dghyp pghyp rghyp qghyp
### Keywords: distribution models multivariate datagen

### ** Examples

  ## Univariate generalized hyperbolic distribution
  univariate.ghyp <- ghyp()

  par(mfrow=c(5, 1))

  quantiles <- seq(-4, 4, length = 500)
  plot(quantiles, dghyp(quantiles, univariate.ghyp))
  plot(quantiles, pghyp(quantiles, univariate.ghyp))

  probabilities <- seq(1e-4, 1-1e-4, length = 500)
  plot(probabilities, qghyp(probabilities, univariate.ghyp, method = "splines"))

  hist(rghyp(n=10000,univariate.ghyp),nclass=100)

  ## Mutivariate generalized hyperbolic distribution
  multivariate.ghyp <- ghyp(sigma=var(matrix(rnorm(10),ncol=2)),mu=1:2,gamma=-(2:1))

  par(mfrow=c(2, 1))

  quantiles <- outer(seq(-4, 4, length = 50), c(1, 1))
  plot(quantiles[, 1], dghyp(quantiles, multivariate.ghyp))
  plot(quantiles[, 1], pghyp(quantiles, multivariate.ghyp, n.sim = 1000))

  rghyp(n = 10, multivariate.ghyp)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("ghyp-get")
### * ghyp-get

flush(stderr()); flush(stdout())

### Name: ghyp-get
### Title: Get methods for objects inheriting from class ghyp
### Aliases: ghyp.data ghyp.name ghyp.fit.info ghyp.dim
### Keywords: utilities

### ** Examples

  ## multivariate generalized hyperbolic distribution
  ghyp.mv <- ghyp(lambda = 1, alpha.bar = 0.1, mu = rep(0, 2), sigma = diag(rep(1, 2)),
                  gamma = rep(0, 2), data = matrix(rt(1000, df = 4), ncol = 2))

  ## Get data
  ghyp.data(ghyp.mv)

  ## Get the dimension
  ghyp.dim(ghyp.mv)

  ## Get the name of the ghyp object
  ghyp.name(ghyp(alpha.bar = 0))
  ghyp.name(ghyp(alpha.bar = 0, lambda = -4), abbr = TRUE)

  ## 'ghyp.fit.info' does only work when the object is of class 'mle.ghyp',
  ## i.e. is created by 'fit.ghypuv' etc.
  mv.fit <- fit.tmv(data = ghyp.data(ghyp.mv), control = list(abs.tol = 1e-3))
  ghyp.fit.info(mv.fit)



cleanEx()
nameEx("ghyp-mle.ghyp-classes")
### * ghyp-mle.ghyp-classes

flush(stderr()); flush(stdout())

### Name: ghyp-mle.ghyp-classes
### Title: Classes ghyp and mle.ghyp
### Aliases: ghyp-class show.ghyp show,ghyp-method mle.ghyp-class
###   show.mle.ghyp show,mle.ghyp-method
### Keywords: classes

### ** Examples

  data(smi.stocks)
  multivariate.fit <- fit.ghypmv(data = smi.stocks,
                                 opt.pars = c(lambda = FALSE, alpha.bar = FALSE),
                                 lambda = 2)
  summary(multivariate.fit)

  vcov(multivariate.fit)
  mean(multivariate.fit)
  logLik(multivariate.fit)
  AIC(multivariate.fit)
  coef(multivariate.fit)

  univariate.fit <- multivariate.fit[1]
  hist(univariate.fit)

  plot(univariate.fit)
  lines(multivariate.fit[2])



cleanEx()
nameEx("ghyp-risk-performance")
### * ghyp-risk-performance

flush(stderr()); flush(stdout())

### Name: ghyp-risk-performance
### Title: Risk and Performance Measures
### Aliases: ESghyp ghyp.omega
### Keywords: utilities misc

### ** Examples

  data(smi.stocks)

  ## Fit a NIG model to Credit Suisse and Swiss Re log-returns
  cs.fit <- fit.NIGuv(smi.stocks[, "CS"], silent = TRUE)
  swiss.re.fit <- fit.NIGuv(smi.stocks[, "Swiss.Re"], silent = TRUE)

  ## Confidence levels for expected shortfalls
  es.levels <- c(0.001, 0.01, 0.05, 0.1)

  cs.es <- ESghyp(es.levels, cs.fit)
  swiss.re.es <- ESghyp(es.levels, swiss.re.fit)

  ## Threshold levels for Omega
  threshold.levels <- c(0, 0.01, 0.02, 0.05)

  cs.omega <- ghyp.omega(threshold.levels, cs.fit)
  swiss.re.omega <- ghyp.omega(threshold.levels, swiss.re.fit)

  par(mfrow = c(2, 1))

  barplot(rbind(CS = cs.es, Swiss.Re = swiss.re.es), beside = TRUE,
          names.arg = paste(100 * es.levels, "percent"), col = c("gray40", "gray80"),
          ylab = "Expected Shortfalls (return distribution)", xlab = "Level")

  legend("bottomright", legend = c("CS", "Swiss.Re"), fill = c("gray40", "gray80"))

  barplot(rbind(CS = cs.omega, Swiss.Re = swiss.re.omega), beside = TRUE,
          names.arg = threshold.levels, col = c("gray40", "gray80"),
          ylab = "Omega", xlab = "Threshold level")
  legend("topright", legend = c("CS", "Swiss.Re"), fill = c("gray40", "gray80"))

  ## => the higher the performance, the higher the risk (as it should be)



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("ghyp.moment")
### * ghyp.moment

flush(stderr()); flush(stdout())

### Name: ghyp.moment
### Title: Compute moments of generalized hyperbolic distributions
### Aliases: ghyp.moment
### Keywords: utilities

### ** Examples

  nig.uv <- NIG(alpha.bar = 0.1, mu = 1.1, sigma = 3, gamma = -2)

  # Moments of integer order
  ghyp.moment(nig.uv, order = 1:6)

  # Moments of fractional order
  ghyp.moment(nig.uv, order = 0.2 * 1:20, absolute = TRUE)



cleanEx()
nameEx("gig-distribution")
### * gig-distribution

flush(stderr()); flush(stdout())

### Name: gig-distribution
### Title: The Generalized Inverse Gaussian Distribution
### Aliases: dgig pgig qgig ESgig rgig Egig
### Keywords: distribution datagen

### ** Examples

dgig(1:40, lambda = 10, chi = 1, psi = 1)
qgig(1e-5, lambda = 10, chi = 1, psi = 1)

ESgig(c(0.19,0.3), lambda = 10, chi = 1, psi = 1, distr = "loss")
ESgig(alpha=c(0.19,0.3), lambda = 10, chi = 1, psi = 1, distr = "ret")

Egig(lambda = 10, chi = 1, psi = 1, func = "x")
Egig(lambda = 10, chi = 1, psi = 1, func = "var")
Egig(lambda = 10, chi = 1, psi = 1, func = "1/x")



cleanEx()
nameEx("hist-methods")
### * hist-methods

flush(stderr()); flush(stdout())

### Name: hist-methods
### Title: Histogram for univariate generalized hyperbolic distributions
### Aliases: hist.ghyp hist-methods hist,ghyp-method
### Keywords: hplot methods

### ** Examples

  data(smi.stocks)
  univariate.fit <- fit.ghypuv(data = smi.stocks[,"SMI"],
                               opt.pars = c(mu = FALSE, sigma = FALSE),
                               symmetric = TRUE)
  hist(univariate.fit)



cleanEx()
nameEx("indices")
### * indices

flush(stderr()); flush(stdout())

### Name: indices
### Title: Monthly returns of five indices
### Aliases: indices
### Keywords: datasets

### ** Examples

  data(indices)

  pairs(indices)



cleanEx()
nameEx("lik.ratio.test")
### * lik.ratio.test

flush(stderr()); flush(stdout())

### Name: lik.ratio.test
### Title: Likelihood-ratio test
### Aliases: lik.ratio.test
### Keywords: utilities

### ** Examples

  data(smi.stocks)

  sample <- smi.stocks[, "SMI"]

  t.symmetric <- fit.tuv(sample, silent = TRUE, symmetric = TRUE)
  t.asymmetric <- fit.tuv(sample, silent = TRUE)

  # Test symmetric Student-t against asymmetric Student-t in case
  # of SMI log-returns
  lik.ratio.test(t.asymmetric, t.symmetric, conf.level = 0.95)
  # -> keep the null hypothesis

  set.seed(1000)
  sample <- rghyp(1000, student.t(gamma = 0.1))

  t.symmetric <- fit.tuv(sample, silent = TRUE, symmetric = TRUE)
  t.asymmetric <- fit.tuv(sample, silent = TRUE)

  # Test symmetric Student-t against asymmetric Student-t in case of
  # data simulated according to a slightly skewed Student-t distribution
  lik.ratio.test(t.asymmetric, t.symmetric, conf.level = 0.95)
  # -> reject the null hypothesis

  t.symmetric <- fit.tuv(sample, silent = TRUE, symmetric = TRUE)
  ghyp.asymmetric <- fit.ghypuv(sample, silent = TRUE)

  # Test symmetric Student-t against asymmetric generalized
  # hyperbolic using the same data as in the example above
  lik.ratio.test(ghyp.asymmetric, t.symmetric, conf.level = 0.95)
  # -> keep the null hypothesis



cleanEx()
nameEx("logLik-AIC-methods")
### * logLik-AIC-methods

flush(stderr()); flush(stdout())

### Name: logLik-AIC-methods
### Title: Extract Log-Likelihood and Akaike's Information Criterion
### Aliases: logLik.mle.ghyp logLik,mle.ghyp-method AIC.mle.ghyp
###   AIC,mle.ghyp-method
### Keywords: utilities methods

### ** Examples

  data(smi.stocks)

  ## Multivariate fit
  fit.mv <- fit.hypmv(smi.stocks, nit = 10)
  AIC(fit.mv)
  logLik(fit.mv)

  ## Univariate fit
  fit.uv <- fit.tuv(smi.stocks[, "CS"], control = list(maxit = 10))
  AIC(fit.uv)
  logLik(fit.uv)

  # Both together
  AIC(fit.uv, fit.mv)
  logLik(fit.uv, fit.mv)



cleanEx()
nameEx("mean-vcov-skew-kurt-methods")
### * mean-vcov-skew-kurt-methods

flush(stderr()); flush(stdout())

### Name: mean-vcov-skew-kurt-methods
### Title: Expected value, variance-covariance, skewness and kurtosis of
### Aliases: mean.ghyp mean-methods mean,ghyp-method vcov.ghyp vcov-methods
###   vcov,ghyp-method ghyp.skewness ghyp.kurtosis
### Keywords: utilities methods

### ** Examples

  ## Univariate: Parametric
  vg.dist <- VG(lambda = 1.1, mu = 10, sigma = 10, gamma = 2)
  mean(vg.dist)
  vcov(vg.dist)
  ghyp.skewness(vg.dist)
  ghyp.kurtosis(vg.dist)

  ## Univariate: Empirical
  vg.sim <- rghyp(10000, vg.dist)
  mean(vg.sim)
  var(vg.sim)

  ## Multivariate: Parametric
  vg.dist <- VG(lambda = 0.1, mu = c(55, 33), sigma = diag(c(22, 888)), gamma = 1:2)
  mean(vg.dist)
  vcov(vg.dist)

  ## Multivariate: Empirical
  vg.sim <- rghyp(50000, vg.dist)
  colMeans(vg.sim)
  var(vg.sim)



cleanEx()
nameEx("pairs-methods")
### * pairs-methods

flush(stderr()); flush(stdout())

### Name: pairs-methods
### Title: Pairs plot for multivariate generalized hyperbolic distributions
### Aliases: pairs.ghyp pairs-methods pairs,ghyp-method
### Keywords: multivariate methods hplot

### ** Examples

  data(smi.stocks)
  fitted.smi.stocks <- fit.NIGmv(data = smi.stocks[1:200, ])
  pairs(fitted.smi.stocks)



cleanEx()
nameEx("plot-lines-methods")
### * plot-lines-methods

flush(stderr()); flush(stdout())

### Name: plot-lines-methods
### Title: Plot univariate generalized hyperbolic densities
### Aliases: plot.ghyp plot,ghyp,missing-method plot-methods lines.ghyp
###   lines-methods lines,ghyp-method
### Keywords: hplot methods

### ** Examples

  data(smi.stocks)

  smi.fit   <-  fit.tuv(data = smi.stocks[,"SMI"], symmetric = TRUE)
  nestle.fit <- fit.tuv(data = smi.stocks[,"Nestle"], symmetric = TRUE)

  ## Student-t distribution
  plot(smi.fit, type = "l", log = "y")
  lines(nestle.fit, col = "blue")

  ## Empirical
  lines(density(smi.stocks[,"SMI"]), lty = "dashed")
  lines(density(smi.stocks[,"Nestle"]), lty = "dashed", col = "blue")



cleanEx()
nameEx("portfolio.optimize")
### * portfolio.optimize

flush(stderr()); flush(stdout())

### Name: portfolio.optimize
### Title: Portfolio optimization with respect to alternative risk measures
### Aliases: portfolio.optimize
### Keywords: optimize multivariate iteration

### ** Examples


data(indices)

t.object <- fit.tmv(-indices, silent = TRUE)
gauss.object <- fit.gaussmv(-indices)

t.ptf <- portfolio.optimize(t.object,
                            risk.measure = "expected.shortfall",
                            type = "minimum.risk",
                            level = 0.99,
                            distr = "loss",
                            silent = TRUE)

gauss.ptf <- portfolio.optimize(gauss.object,
                                risk.measure = "expected.shortfall",
                                type = "minimum.risk",
                                level = 0.99,
                                distr = "loss")

par(mfrow = c(1, 3))

plot(c(t.ptf$risk, gauss.ptf$risk),
     c(-mean(t.ptf$portfolio.dist), -mean(gauss.ptf$portfolio.dist)),
     xlim = c(0, 0.035), ylim = c(0, 0.004),
     col = c("black", "red"), lwd = 4,
     xlab = "99 percent expected shortfall",
     ylab = "Expected portfolio return",
     main = "Global minimum risk portfolios")

legend("bottomleft", legend = c("Asymmetric t", "Gaussian"),
       col = c("black", "red"), lty = 1)

plot(t.ptf$portfolio.dist, type = "l",
     xlab = "log-loss ((-1) * log-return)", ylab = "Density")
lines(gauss.ptf$portfolio.dist, col = "red")

weights <- cbind(Asymmetric.t = t.ptf$opt.weights,
                 Gaussian = gauss.ptf$opt.weights)

barplot(weights, beside = TRUE, ylab = "Weights")




graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("qq-ghyp")
### * qq-ghyp

flush(stderr()); flush(stdout())

### Name: qq-ghyp
### Title: Quantile-Quantile Plot
### Aliases: qqghyp
### Keywords: models hplot

### ** Examples

  data(smi.stocks)

  smi <- fit.ghypuv(data = smi.stocks[, "Swiss.Re"])

  qqghyp(smi, spline.points = 100)

  qqghyp(fit.tuv(smi.stocks[, "Swiss.Re"], symmetric = TRUE),
         add = TRUE, ghyp.col = "red", line = FALSE)



cleanEx()
nameEx("scale-methods")
### * scale-methods

flush(stderr()); flush(stdout())

### Name: scale-methods
### Title: Scaling and Centering of ghyp Objects
### Aliases: scale.ghyp scale,ghyp-method
### Keywords: utilities methods

### ** Examples

  data(indices)

  t.fit <- fit.tmv(indices)
  gauss.fit <- fit.gaussmv(indices)

  ## Compare the fitted Student-t and Gaussian density.
  par(mfrow = c(1, 2))

  ## Once on the real scale...
  plot(t.fit[1], type = "l")
  lines(gauss.fit[1], col = "red")

  ## ...and once scaled to expectation = 0, variance = 1
  plot(scale(t.fit)[1], type = "l")
  lines(scale(gauss.fit)[1], col = "red")



graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("smi.stocks")
### * smi.stocks

flush(stderr()); flush(stdout())

### Name: smi.stocks
### Title: Daily returns of five swiss blue chips and the SMI
### Aliases: smi.stocks
### Keywords: datasets

### ** Examples

  data(smi.stocks)

  pairs(smi.stocks)



cleanEx()
nameEx("stepAIC.ghyp")
### * stepAIC.ghyp

flush(stderr()); flush(stdout())

### Name: stepAIC.ghyp
### Title: Perform a model selection based on the AIC
### Aliases: stepAIC.ghyp
### Keywords: utilities

### ** Examples


  data(indices)

  # Multivariate case:
  aic.mv <- stepAIC.ghyp(indices, dist = c("ghyp", "hyp", "t", "gauss"),
                         symmetric = NULL, control = list(maxit = 500),
                         silent = TRUE, nit = 500)

  summary(aic.mv$best.model)

  # Univariate case:
  aic.uv <- stepAIC.ghyp(indices[, "stock"], dist = c("ghyp", "NIG", "VG", "gauss"),
                         symmetric = TRUE, control = list(maxit = 500), silent = TRUE)


  # Test whether the ghyp-model provides a significant improvement with
  # respect to the VG-model:
  lik.ratio.test(aic.uv$all.models[[1]], aic.uv$all.models[[3]])




cleanEx()
nameEx("summary-method")
### * summary-method

flush(stderr()); flush(stdout())

### Name: summary-method
### Title: mle.ghyp summary
### Aliases: summary.mle.ghyp summary-methods summary,mle.ghyp-method
### Keywords: methods

### ** Examples

  data(smi.stocks)
  mle.ghyp.object <- fit.NIGmv(smi.stocks[, c("Nestle", "Swiss.Re", "Novartis")])
  summary(mle.ghyp.object)



cleanEx()
nameEx("transform-extract-methods")
### * transform-extract-methods

flush(stderr()); flush(stdout())

### Name: transform-extract-methods
### Title: Linear transformation and extraction of generalized hyperbolic
### Aliases: transform.ghyp transform,ghyp-method [.ghyp
###   [,ghyp,numeric,missing,missing-method
### Keywords: utilities methods

### ** Examples

  ## Mutivariate generalized hyperbolic distribution
  multivariate.ghyp <- ghyp(sigma=var(matrix(rnorm(9),ncol=3)), mu=1:3, gamma=-2:0)

  ## Dimension reduces to 2
  transform(multivariate.ghyp, multiplier=matrix(1:6,nrow=2), summand=10:11)

  ## Dimension reduces to 1
  transform(multivariate.ghyp, multiplier=1:3)

  ## Simple transformation
  transform(multivariate.ghyp, summand=100:102)

  ## Extract some dimension
  multivariate.ghyp[1]
  multivariate.ghyp[c(1, 3)]



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
