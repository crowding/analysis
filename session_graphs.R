#!usr/bin/env Rscript
source("common.manipulations.R")

session.graphs <- function(...) {
  library(plyr)
  library(lattice)
  my.args <- list(...)

  load(my.args[[1]])

  ##for the various quest experiments, work out what was changing.my.ar
  conditions <- what.varies(runs)

  #do the basic assignments. I am lazy and will just pass the whole environment down to work on:
  common.manipulations(environment())
  
  ##Ah! See how I can put the "close PDF" right next to the "open PDF"
  ##like it SHOULD be.
  pdf(file=my.args[[2]], onefile=TRUE)
  tryCatch(finally=dev.off(), {
    if (any(trials$motionCondition=="incongruent", na.rm=TRUE)) {
      ##come up with a grouping expession.
      grouping <- eval(parse(text=paste("responseTime ~ jitter(trial.extra.nTargets) | ", paste(c("trial.motion.process.radius", conditions), collapse=" * "))))
      groupingLabels <- c("eccentricity", sub("^.*\\.","", conditions))
      ##show each response plotted vs. radius and other parameters....
      all.close <- function(x,...) isTRUE(all.equal(x, rep(median(x),length(x)), ...))
      print(xyplot(grouping,
                   trials[trials$motionCondition == "incongruent",],
                   groups=correct,        
                   auto.key=TRUE,
                   ylim=c(0,1.2),
                   strip = strip.custom(
                     strip.levels = c(TRUE,TRUE),
                     var.name = groupingLabels),
                   panel=function(x, y, ... ) {
                     panel.xyplot(x, y, ...)
                     responses <- !is.na(trials$result.response)
                     if (all.close(trials[responses,"minResponseTime"])) {
                       panel.refline(h = median(trials[responses,"minResponseTime"]))
                     }
                     if (all.close(trials[responses,"maxResponseTime"])) {
                       panel.refline(h = median(trials[responses,"maxResponseTime"]))
                     }
                   })
            )
      ##plot each response as a function of time also, to see QUEST working.
      grouping <- eval(parse(text=paste("trial.extra.nTargets ~ result.endTime | ", paste(c("trial.motion.process.radius", conditions), collapse=" * "))))
      print(xyplot(grouping,
                   trials[trials$motionCondition == "incongruent",],
                   groups=correct,        
                   auto.key=TRUE,
                   strip = strip.custom(
                     strip.levels = c(TRUE,TRUE),
                     var.name = groupingLabels)
                   ))
    }
  })
}

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call(session.graphs, as.list(my.args))
}
