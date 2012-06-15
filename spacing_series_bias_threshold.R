suppressPackageStartupMessages({
	library(ggplot2)
        library(plyr)
        library(ptools)
        library(stringr)
        library(psyphy)
        source("db_functions.R")
        source("modeling.manipulations.R")
})

columns.to.pull <- c("trial.motion.process.radius",
                     "trial.extra.side",
                     "trial.extra.nVisibleTargets",
                     "abs.displacement",
                     "abs.localDirectionContrast",
                     "trial.extra.tf",
                     "trial.extra.wavelengthScalar",
                     "trial.extra.dt",
                     "trial.extra.widthScalar",
                     "trial.extra.durationScalar",
                     "trial.extra.nTargets",
                     "trial.motion.process.order",
                     "trial.motion.process.n",
                     "visibilityCondition",
                     "folded.localDirectionContrast",
                     "folded.displacement",
                     "folded.response",
                     "abs.response",
                     "target.spacing",
                     "responseInWindow",
                     "responseTime",
                     "maxResponseTime",
                     "loaded.from",
                     "runs.i",
                     "subject"
                     )

#main("collections/spacing_series.list", "discrimination.sqlite", "collections/spacing_series_bias_threshold.pdf")
#trials <- pull.from.db(conn, data.frame(loaded.from=str_trim(readLines("collections/spacing_series.list"))))
main <- function(flist, dbfile, output) {
  drv <- SQLite()
  files <- str_trim(readLines(flist))
  trials <- with.db.connection(drv, dbfile, fn=function(conn) {
    pull.from.db(conn, data.frame(loaded.from="flist"))
  })
  measure.thresholds(trials, output)
}

measure.thresholds <- function(trials, output) {

  split <- c(  "subject"
             , "target_spacing"
             , "trial_motion_process_radius"
             , "abs_localDirectionContrast"
             )
  used <- c("abs_displacement", "abs_response", "responseInWindow")
  ddply(trials[union(split, used)], split, psychometric.function, .progress="text")
}

psychometric.function <- function(data) {
  #return slope, threshold, and quantiles of each from simulation
  data <- mutate(data, response.cw = abs_response > 0)
  fit <- glm(  response.cw ~ abs_displacement
             , binomial(link=logit.2asym(0.05, 0.05))
             , subset(data, as.logical(responseInWindow))
             )

  ##note this will find a bunch of intercepts (arg 2) if you want...
  cases <- data[1,,drop=FALSE]
  X <- find.intercept.glm(  fit, cases
                                   , 'abs_displacement'
                                   , response = c(.5, .75)
                                   , result.type="list"
                                   , sims=500
                                   )

  with(  X
       , c(  bias = intercept[[1]]
           , c(bias = quantile(sim[[1]], c(0.1,0.25,0.50,0.75,0.9)))
           , threshold = intercept[[2]] - intercept[[1]]
           , c(threshold = quantile(sim[[2]] - sim[[1]], c(0.1,0.25,0.50,0.75,0.9)))
           )
       )
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("main", as.list(args))
}
