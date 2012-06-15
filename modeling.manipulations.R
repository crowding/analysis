library(arm)

##Function to compute the PSE estimate and the slope of the
##psychometric function at the PSE. Note that this amounts to
##finding the x-intercept of the regression line.
find.intercept.glm <- function(model, cases, varname, response=0.5,
                               test.values=c(1,2),
                               response.name, slope.name="slope", coef=NULL,
                               result.type= "data.frame", sims=0) {
  ## Compute the slope and PSE for each condition that is listed in
  ## 'cases.'  I do this by computing the (pre-link) response for
  ## two test values for each condition (conditions listed in
  ## 'cases.')

  # I just realized this will have less copying if the simulation is
  # done where the "coef" subsetting is done. So do that.
  if (sims > 0) {
    fit.sim <- sim(model, sims)
  }
  
  if (!is.null(coef)) {
    model$coefficients <- coef
  }
  
  yval <- model$family$linkfun(response)

  case1 <- cases
  case2 <- cases
  case1[,varname] <- test.values[[1]]
  case2[,varname] <- test.values[[2]]
  link1 <- predict(model, newdata=case1, type="link")
  link2 <- predict(model, newdata=case2, type="link")
  slope <- (link2-link1)*(test.values[[2]]-test.values[[1]])
  xintercept <- test.values[[1]] - (link1-yval)/slope
    
  result <- switch(result.type,
         data.frame={
           cases[, varname] <- xintercept
           cases[, response.name] <- response
           cases[, slope.name] <- slope
           cases
         },
         list = list(intercept=xintercept)
         )

  if (sims > 0) {
    simulations <- sim(model, sims)
    intercepts <- array(0, c(sims, length(response)))

    # arrange the coefficient matrix so that we can get the intercepts
    # with a big matrix multiplication.
    tt <- terms(fit)
    Terms <- delete.response(tt)
    m1 <- model.frame(Terms, case1, na.action=na.pass, xlev=fit$xlevels)
    m2 <- model.frame(Terms, case2, na.action=na.pass, xlev=fit$xlevels)
    X1 <- model.matrix(Terms, m1, contrasts.arg = fit$contrasts)
    X2 <- model.matrix(Terms, m2, contrasts.arg = fit$contrasts)        

    link1 <- X1 %*% t(simulations@coef)
    link2 <- X2 %*% t(simulations@coef)

    slope = (link2 - link1) / (test.values[[2]] - test.values[[1]])
    xintercept <- llply(yval, function(y) test.values[[1]] - (link1 - y)/slope)

    switch(result.type, {
      list={result <- c(result, list(sim=xintercept))}
    })
  }
  result
}

fit.and.simulate <- function(fmla, data=trials[considered.trials,], iter=200, cond=conditions) {
  counter <<- 0
  ## This implements a simulation-based, fixed-X bootstrap. This is
  ## a bootstrap based on simulating a random response according to
  ## the fitted model and its residual error term, using the same
  ## values for predictor variables as in the original bootstrap.
  ##
  ## The more usual kind of bootstrap is case resampling, where you
  ## generate a resampling of the distribution by drawing from the
  ## original dataset with replacement. I'm not doing that for a few
  ## reasons: later on I will be fitting QUEST data, where the
  ## samples are not independently drawn (which causes a case-based
  ## bootstrap to blow up) Second, a simulation-based bootstrap
  ## estimate of standard error of a linear function of the
  ## regression coefficients should converge to the same answer as
  ## the usual inverse Hessian trick. (this should be clear, as the
  ## standard error you compute in a linear regression answers the
  ## question "how much would the estimate of this parameter vary if
  ## the data were drawn randomly from the fitted model?")  So it
  ## can be interpreted in the same way as standard errors from
  ## linear models can be. Third, a fixed-X estimate will tend to
  ## underestimate the variance of the estimates in case of
  ## overdispersion or heteroskedasticity. This can be good: I want
  ## to go on to compare the data residuals with the estimated
  ## standard errors, to see where the data has unexplained
  ## variance.
  ##
  ## references: \cite{Gelman:2007fr} (chapter 7) and
  ## \cite{Fox:2002zr}
  model <- fit.model(data, formula=fmla)
  cond <- find.intercept(model, cond, 'log.target.spacing',
                         response=0.5, response.name='pCorrect')
  counter <- 0
  pb <- txtProgressBar(min=0, max=iter)
  tryCatch(finally=close(pb), {
    ##The simulation
    fit.sim <- sim(model, iter)
    
    ##this gives a list of fitted coefficients. Translate those
    ##into the PSEs and their errors.
    model2 <- model
    
    ## workround for a slowness issue with ddply....
    cond$..cond.i <- 1:nrow(cond)
    sim.intercepts <- adply(fit.sim@coef, 1, function(s) {
      counter <<- counter+1
      setTxtProgressBar(pb, counter)
      model2$coefficients <- s;
      find.intercept(model2, cond, 'log.target.spacing',
                     response=0.5, response.name='pCorrect')
    })
    
    ##cond.sds <- profr(ddply(sim.intercepts, colnames(cond), function(s) {
    cond.sds <- ddply(sim.intercepts, "..cond.i", function(s) {
      c(log.target.spacing.sd=sd(s$log.target.spacing), slope.sd=sd(s$slope))
    })
    cond <- merge(cond, cond.sds)
    cond$..cond.i <- NULL
  })
  
  return(list(model=model, conditions=cond, sim=fit.sim))
}         

sim.statistic.glm <- function(model, fn, .progress="none", ...) {
  adply(model$sim@coef, 1, function(row) {
    model$coefficients[] <- row
    fn(model, ...)
  }, .progress=.progress)
}

sim.statistic <- function(model, fn, ...) UseMethod("sim.statistic")
