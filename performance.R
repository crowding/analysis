suppressPackageStartupMessages({
  require(ggplot2)
  require(plyr)
  require(gridExtra)
  source("programming.R")
})

process <- function(trials, triggers, output, ...) {
  ##Produce some basic stats about the day's session (fixation breaks, ...)


  theme_set(theme_bw())
  theme_update(panel.grid.major = theme_blank(),
               panel.grid.minor = theme_blank())

  ##scan triggers for fixation breaks, etc.
  ddply(  triggers, "trials.i", summarize
        , started = started <- "ConcentricTrial/run/startMotion" %in% name
        , startTime = if (started) triggerTime[name=="ConcentricTrial/run/startMotion"] else NA
        , fixation.break = fixation.break <- started && ("ConcentricTrial/run/failedFixation" %in% name)
        , responded = responded <- any(name %in% c("ConcentricTrial/run/ccw", "ConcentricTrial/run/cw"))
        , early = early <- "ConcentricTrial/run/tooLong" %in% name
        , late = late <- "ConcentricTrial/run/tooShort" %in% name
        , completed = started && !fixation.break && !early && !late
        ) -> disposition

  pipe(trials
       , subset(select = c("i", "responseTime", "subject", "source.file"))
       , merge(disposition, by.x = "i", by.y = "trials.i")
       , ddply(c("subject", "source.file")
               , summarise
               , date=substr(source.file[1],
                   match <- regexpr('([0-9]{4})',source.file[1]),
                   match+9)
               , started = sum(started)
               , fixation.breaks = sum(fixation.break)
               , early = sum(early)
               , late = sum(late)
               , completed = sum(completed)
               , rt.quantiles = list(quantile(na.exclude(responseTime), probs=c(0.25, 0.50, 0.75)))
               , fixation.breaks.p = sum(fixation.break) / sum(started)
               , early.p = sum(early) / sum(started)
               , late.p = sum(late) / sum(started)
               , completed.p = sum(completed) / sum(started)
               , pace = mean(diff(startTime[completed]))
               , pace.per.hr = 3600/mean(diff(startTime[completed]))
               )
       ) -> summary

       summary$source.file <- NULL

  grid.newpage()
  
  summary.print <- colwise(function(x)format(x, nsmall=2))(summary)
  #whaaaat the hell, why doesn't it format????
  grid.table(summary.print
             , padding.v = unit(2,"mm"), padding.h = unit(2,"mm")
             , gp = gpar(fontsize=7)
             )          
}
