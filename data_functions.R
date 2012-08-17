suppressPackageStartupMessages({
  source("modeling.manipulations.R")
  source("helper_functions.R")
  source("graphics_functions.R")
})

`%-%` <- setdiff
`%+%` <- union

exclude.cols <-  c(  "abs_displacement", "folded_displacement"
                   , "visibilityCondition"
                   , "folded_response", "abs_response", "trial_extra_nTargets"
                   , "responseInWindow", "responseTime", "maxResponseTime"
                   , "loaded_from", "runs_i", "trial_i")

measure_thresholds <- function(  trials
                               , per_session=FALSE
                               , use_folded=TRUE
                               , ...
                               , split = colnames(trials) %-% exclude.cols) {

  if (per_session) split <- split %+% "loaded_from"

  used <- c("responseInWindow")

  if (use_folded) {
    split <- split %-% c("abs_localDirectionContrast"
                         , "trial_extra_content_ccw", "trial_extra_content_cw")
    used <- used %+% c("folded_displacement", "folded_response")
  } else {
    split <- split %-% c("folded_localDirectionContrast"
                         , "folded_content_with", "folded_content_against")
    used <- used %+% c("abs_displacement", "abs_response")
  }
  
  ddply(trials, split, psychometric_function, .progress="text",
        use_folded=use_folded, ...)
}

has_both_signs <- function(x) diff(range(sign(x))) == 2

psychometric_function <-
  function(  data, average_bias=TRUE, use_folded=TRUE, abs_bias=TRUE
           , sims=500
           , one_sided = use_folded && !has_both_signs(data$abs_localDirectionContrast)
           , plot=FALSE) {
  cont <- list()

  data$n <- nrow(data)
  
  #return slope, threshold, and quantiles of each from simulation
  if (use_folded) {
    data <- mutate(data, folded_response = folded_response > 0)
    formula <- folded_response ~ folded_displacement
    var <- "folded_displacement"
    if (abs_bias) {
      ##Try to account for absolute(clockwise/counterclockwise)
      ##biases. This might improve the estimate we care about.
      ##I can't figure how to interpret intercepts or whatnot here.
      data <- mutate(data, not_folded =
                     ifelse(abs_displacement == folded_displacement, 0.5, -0.5))
      formula <- update(formula, . ~ . + not_folded)
      #cont$not_folded <- contr.sum
    }
  } else {
    data <- mutate(  data, abs_response = abs_response > 0)
    formula <- abs_response ~ abs_displacement
    var <- "abs_displacement"
  }

  if (one_sided) {
    ##In folded data without any motion direction content, we have to
    ##assume symmetry; the "folded" function must go through 0. abs_bias takes 
    formula <- update(formula, . ~ . - 1)
  }

  n_sessions <- length(unique(data$loaded_from))
  
  if (average_bias && n_sessions > 1) {
    ##we allow the bias to vary by session, and measure the average
    ##bias by specifying the contrast in sum form.  Note that I
    ##could make this more parsimonious by letting there only be one
    ##adjustment between pairs of sessions.
    data <- mutate(data, loaded_from = factor(loaded_from))
    formula <- update(formula, . ~ . + loaded_from)
    cont$loaded_from <- "contr.sum"
  }
   
  fit <- glm(  formula
             , binomial(link=logit.2asym(0.05, 0.05))
             , subset(data, as.logical(responseInWindow))
             , contrasts=if (length(cont) > 0) cont else NULL
             )
  
  ##note this will find a bunch of intercepts (arg 2) if you want...
  ##Note also that we are finding the intercepts for the aceraged data,
  cases <- data[1,,drop=FALSE]
  cases['not_folded'] <- 0;

  X <- find.intercept.glm(  fit, cases
                                   , var
                                   , response = c(.5, .75)
                                   , result.type="list"
                                   , sims=sims
                                   , average.over=if (n_sessions > 1) "loaded_from" else c()
                                   )

  ##And we cons up all the measurements we want for this psychometric function.
  measurements <- with( X
       , c(  xint = intercept[[1]]
           , if (sims>0) {
               c(xint = quantile(sim[[1]], c(0.1,0.25,0.50,0.75,0.9), na.rm=TRUE))
             } else c()
           , if (sims>0) {
               c(threshold = quantile(sim[[2]] - sim[[1]],
                                      c(0.1,0.25,0.50,0.75,0.9),
                                      na.rm=TRUE))
             } else c()
           , threshold = intercept[[2]] - intercept[[1]]
           , if(one_sided) {
               c( yint=0, yint.sd=0
                 )
             } else {
               c(  yint = fit$coefficients[["(Intercept)"]]
                 , yint.sd = ifelse(  !is.na(fit$coefficients[["(Intercept)"]])
                                    , sqrt(vcov(fit)[["(Intercept)","(Intercept)"]])
                                    , NA)
                 )
             }
           , slope = fit$coefficients[[var]]
           , slope.sd = if (!is.na(fit$coefficients[[var]])) {
               sqrt(vcov(fit)[[var,var]])
             } else NA
           , if (sims>0) {
               c(slope.positive = mean(sim[[2]] - sim[[1]] > 0))
             } else c()
           , n = nrow(data)
           , if ( abs_bias ) {
             c(  cw.bias = fit$coefficients[["not_folded"]]
               , cw.bias.sem =
                   ifelse(is.na(fit$coefficients[["not_folded"]])
                          , NA, sqrt(vcov(fit)[["not_folded","not_folded"]])))
             } else c()
           )
  )

  ##generate a plot of the raw data, the curve fit, and the bias,
  ##threshold and slope.
  if(plot) do.call(pmetric_plot, as.list(environment()))

  if ( isTRUE(with(as.list(measurements), sign(xint) != -sign(yint)*sign(slope))) ) {
    stop("This slope, bias and x-intercept do not make sense!")
  }

  measurements
}

