suppressPackageStartupMessages({
  source("modeling.manipulations.R")
})

`%-%` <- setdiff
`%+%` <- union

exclude.cols <-  c(  "abs_displacement", "abs_localDirectionContrast"
                   , "visibilityCondition", "folded_displacement"
                   , "folded_response", "abs_response", "trial_extra_nTargets"
                   , "responseInWindow", "responseTime", "maxResponseTime"
                   , "loaded_from", "runs_i")

measure_thresholds <- function(  trials
                               , per_session=FALSE
                               , ...
                               , split = colnames(trials) %-% exclude.cols) {

  if (per_session) split <- split %+% "loaded_from"
  used <- c("folded_displacement", "folded_response", "responseInWindow")
  ddply(trials[split %+% used], split, psychometric_function, .progress="text", ...)
}

psychometric_function <- function(data, average_bias=TRUE, sims=500) {
  #return slope, threshold, and quantiles of each from simulation
  data <- mutate(data, response.cw = folded_response > 0)

  if (average_bias & (n_sessions <- length(unique(data$loaded_from))) > 0) {
    ##we allow the bias to vary by session, and measure the average
    ##bias by specifying the contrast in sum form.  Note that I
    ##could make this more parsimonious by letting there only be one
    ##adjustment between pairs of sessions.
    data <- mutate(data, loaded_from = factor(loaded_from))
    
    fit <- glm(  response.cw ~ folded_displacement + loaded_from
               , binomial(link=logit.2asym(0.05, 0.05))
               , subset(data, as.logical(responseInWindow))
               , contrasts=list(loaded_from="contr.sum"))
  } else {
    
    fit <- glm(  response.cw ~ folded_displacement
               , binomial(link=logit.2asym(0.05, 0.05))
               , subset(data, as.logical(responseInWindow))
               )
  }
  
  ##note this will find a bunch of intercepts (arg 2) if you want...
  cases <- data[1,,drop=FALSE]
  X <- find.intercept.glm(  fit, cases
                                   , 'folded_displacement'
                                   , response = c(.5, .75)
                                   , result.type="list"
                                   , sims=sims
                                   )

    with(  X
       , c(  xint = intercept[[1]]
           , if (sims>0) c(xint = quantile(sim[[1]], c(0.1,0.25,0.50,0.75,0.9))) else c()
           , if (sims>0) c(threshold = quantile(sim[[2]] - sim[[1]], c(0.1,0.25,0.50,0.75,0.9))) else c()
           , threshold = intercept[[2]] - intercept[[1]]
           , yint = fit$coefficients[[1]]
           , yint.sd = sqrt(vcov(fit)[[1,1]])
           , slope = fit$coefficients[[2]]
           , slope.sd = sqrt(vcov(fit)[[2,2]])
           , if (sims>0) slope.positive <- mean(sign(X$sim[[2]])) else c()
           )
       )

}

replace_extension <- function(filename, new_extension) {
  sub(  "((.)\\.[^.]*|)$"
      , paste("\\2.", new_extension, sep="")
      , filename)
}
