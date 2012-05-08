#!/bin/Rscript
suppressPackageStartupMessages({
  source("common.manipulations.R")
  source("programming.R")
})

#extract trial-identifying information and eye data only into a separate file...

load.downsampled <- function(filename) {
  print(filename)
  load(filename)
  common.manipulations(environment())

  ##so downsample the the eye position traces.
  ##first align to motion onset, downsample to 25 msec bins, and trim...
  mutate(trials,
         downsampled=mapply(
           function(d, o) {
             ##average in 10 ms bins referenced to motion onset.
             d <- d - c(0,0,o)
             t.bin <- round_any(d[3,], 0.025)
             cbind(apply(d,1,by,t.bin,mean), unique(t.bin))
           },
           trial.eyeData, motionBegun)
         ) -> trials

  pipe(  trials
         , subset(sapply(downsampled, length) > 1,
                  select = c("i", "downsampled"))
         , mutate(  eye.x = lapply(downsampled, `[`,,1)
                  , eye.y = lapply(downsampled, `[`,,2)
                  , eye.t = lapply(downsampled, `[`,,4)
                  , i = mapply(rep, i, sapply(eye.x, length))
                  , downsampled = NULL
                  , n = NULL
                  )
         , colwise(function(x) do.call(c, x))(.)
         ) -> traces
   
  ##then expand it into a BIG table....
  pipe(  trials
       , subset(  sapply(downsampled, length) > 1
                , select=c("trial.extra.globalDirection", "trial.extra.localDirection"
                    , "trial.extra.nTargets", "trial.extra.nVisibleTargets"
                    , "i", "target.spacing", "responseTime", "trial.extra.side"
                    , "subject", "motionCondition", "result.response","source.file", "runs.i"))
       , mutate(trials.i = i)
       ) -> trials
  list(trials=trials, traces=traces)
}

#filenames <- segment.eye.filenames[1:2]
load.eyedata <- function(...) {
  require(plyr)
  filenames <- c(...)
  output <- filenames[length(filenames)]
  filenames <- filenames[-length(filenames)]
  ##
  loaded <- sapply(filenames, load.downsampled, simplify=F)
  ##instigate some offsets for trial id numbers
  i.offset <- 0
  loaded <- llply(loaded, function(d) {
    omin <- min(c(d$trials$i,0))
    d$trials$i <- d$trials$i + i.offset - omin + 1
    d$traces$i <- d$traces$i + i.offset - omin + 1
    i.offset <<- max(c(i.offset,d$trials$i))
    rownames(d$traces) <- NULL
    rownames(d$trials) <- NULL
    d
  })
  names(loaded) <- NULL
  trials <- do.call(rbind, lapply(loaded, fn(.$trials)))
  traces <- do.call(rbind, llply(loaded, fn(.$traces)))
  save(trials, traces, file=output)
}

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call("load.eyedata", as.list(my.args))
}



eye.movements <- function(...) {

  require(plyr)
  require(ggplot2)
  require(signal)

  load(my.args[[1]])
  common.manipulations(environment())

  #so let's just look at this eye position traces.
  #first align to motion onset, downsample to 100/sec, and trim...
  mutate(trials,
         downsampled=mapply(
           function(d, o) {
             ##average in 10 ms bins referenced to motion onset.
             d <- d - c(0,0,o)
             t.bin <- round_any(d[3,], 0.01)
             cbind(apply(d,1,by,t.bin,mean), unique(t.bin))
           },
           trial.eyeData, motionBegun)
         ) -> trials

  #then expand it into a BIG table....
  pipe(  trials
       , subset(  sapply(downsampled, length) > 1
                , select=c("trial.extra.globalDirection", "trial.extra.localDirection"
                           , "trial.extra.nTargets", "trial.extra.nVisibleTargets"
                           , "i", "target.spacing", "responseTime", "trial.extra.side"
                           , "subject", "motionCondition"))
       ) -> basic

  pipe(  trials
       , subset(sapply(downsampled, length) > 1,
                select = c("i", "downsampled"))
       , mutate(  eye.x = lapply(downsampled, `[`,,1)
                , eye.y = lapply(downsampled, `[`,,2)
                , eye.t = lapply(downsampled, `[`,,4)
                , i = mapply(rep, i, sapply(eye.x, length))
                , downsampled = NULL
                , n = NULL
                )
       , colwise(function(x) do.call(c, x))(.)
       ) -> traces

  ##now we can make some graphs.

  ##average eye traces for a group of trials. Vertically align to motion onset as well.
  ##Note that we drop blinks (NaN) from the data.
  eye.averages <-
    mkpipe(  subset(select="i")
           , unique
           , merge(traces)
           , ddply(  "i"
                   , function(d) {
                       ix <- which(d$eye.t==0)
                       numcolwise(function(x) x - x[ix]) (d)
                     }
                   )
           , subset(!is.nan(eye.x))
           , ddply("eye.t", mean.and.sd)
           )

  pipe(  basic
       , mutate(grouping = interaction(trial.extra.globalDirection))
       , ddply(c("grouping", "trial.extra.side", "subject"), eye.averages)
       , (  ggplot(.)
          + aes(eye.t, mean.eye.y, color=grouping)
          + geom_line()
          + scale_x_continuous(lim=c(-.15, .5))
          + coord_cartesian(ylim=c(-1,1))
          + facet_grid(subject ~ trial.extra.side)
          )
       )

  pipe(  basic
       , mutate(grouping = interaction(trial.extra.globalDirection))
       , ddply(c("grouping", "trial.extra.side", "subject"), eye.averages)
       , (  ggplot(.)
          + aes(eye.t, mean.eye.y, color=grouping)
          + geom_line()
          + scale_x_continuous(lim=c(-.15, .5))
          + coord_cartesian(ylim=c(-1,1))
          + facet_grid(subject ~ trial.extra.side)
          )
       )

  pipe(  basic
       , ddply(  c(  "motionCondition", "trial.extra.globalDirection"
                   , "trial.extra.side", "subject")
               , eye.averages)
       , (  ggplot(.)
          + aes(eye.t, mean.eye.y,
                color=trial.extra.globalDirection, linetype=motionCondition)
          + geom_line()
          + scale_x_continuous(lim=c(-.15, .5))
          + coord_cartesian(ylim=c(-1,1))
          + facet_grid(subject ~ trial.extra.side)
          )
       )
  
  #something something microsaccade analysis...
}
