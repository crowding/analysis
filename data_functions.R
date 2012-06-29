suppressPackageStartupMessages({
  source("modeling.manipulations.R")
})

measure_thresholds <- function(trials,per_session=FALSE) {

  split <- c(  "subject"
             , "target_spacing"
             , "trial_motion_process_radius"
             , "folded_localDirectionContrast"
             )
  if (per_session) split <- union(split, "loaded_from")
  used <- c("folded_displacement", "folded_response", "responseInWindow")
  ddply(trials[union(split, used)], split, psychometric_function, .progress="text")
}

psychometric_function <- function(data) {
  #return slope, threshold, and quantiles of each from simulation
  data <- mutate(data, response.cw = folded_response > 0)
  fit <- glm(  response.cw ~ folded_displacement
             , binomial(link=logit.2asym(0.05, 0.05))
             , subset(data, as.logical(responseInWindow))
             )

  ##note this will find a bunch of intercepts (arg 2) if you want...
  cases <- data[1,,drop=FALSE]
  X <- find.intercept.glm(  fit, cases
                                   , 'folded_displacement'
                                   , response = c(.5, .75)
                                   , result.type="list"
                                   , sims=500
                                   )

  with(  X
       , c(  bias = intercept[[1]]
           , c(bias = quantile(sim[[1]], c(0.1,0.25,0.50,0.75,0.9)))
           , threshold = intercept[[2]] - intercept[[1]]
           , c(threshold = quantile(sim[[2]] - sim[[1]], c(0.1,0.25,0.50,0.75,0.9)))
           , yint = fit$coefficients[[1]]
           , yint.sd = sqrt(vcov(fit)[[1,1]])
           , slope = fit$coefficients[[2]]
           , slope.sd = sqrt(vcov(fit)[[2,2]])
           , slope.positive <- mean(sign(X$sim[[2]]))
           )
       )

}

replace_extension <- function(filename, new_extension) {
  sub(  "((.)\\.[^.]*|)$"
      , paste("\\2.", new_extension, sep="")
      , filename)
}
