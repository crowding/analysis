#here we compute some aggregate numbers about the stimuli from the database.

library(plyr)
library(stringr)
source("db_functions.R")
load("descriptions/descriptions.RData")
drv <- SQLite()
dbfile <- "discrimination.sqlite"

#what is the resolusion and refresh rate?

#answer questions about the trial set used in experiment 1
main <- function(dbfile, outfile) {

  results.files <- chain(c("collections/content_series.list",
                           "collections/spacing_series.list"),
                         lapply(readLines),
                         c %()% .,
                         str_trim)

  segment.files <- chain("collections/numdensity_series.list",
                         lapply(readLines),
                         c %()% .,
                         str_trim)

  numbers <- list()

  calib <- local({
    load("unpacked/pbm-2012-03-23__15-04-11-ConcentricAdjustmentPeriodic.RData")
    with(runs, list(
        resolution=beforeRun.params.cal.rect[[1]]
        , size=(beforeRun.params.cal.spacing[[1]]
                * beforeRun.params.cal.rect[[1]][c(3,4)])
        , distance=beforeRun.params.cal.distance
        , interval=beforeRun.params.cal.interval
        , luminance=beforeRun.params.cal.calibration.stage2.readings[[1]]
        , gamma=beforeRun.params.cal.gamma[[1]]
        ))
    })

  #what are the resolutions, refresh rates, etc...
  with.db.connection(drv, dbfile, fn=function(conn) {
    df <- pull.from.db(
      conn, data.frame(loaded_from=results.files),
      columns=c(
          "trial_extra_r", "trial_extra_min_distance", "trial_extra_dt",
          "trial_motion_process_velocity", "trial_motion_process_wavelength",
          "trial_motion_process_width", "trial_extra_tf",
          "trial_motion_process_n",
          "trial_extra_content_ccw", "trial_extra_content_cw",
          "subject", "trial_awaitInput", "trial_maxResponseLatency",
          "trial_fixationSettle", "trial_motion_process_t"))
    numbers$dt <<- unique(df$trial_extra_dt)
    numbers$tf <<- unique(with(df, abs(trial_extra_tf)))
    numbers$sf <<- unique(1/df$trial_motion_process_wavelength)
    numbers$width <<- unique(df$trial_motion_process_width)
    numbers$eccentricity <<- unique(df$trial_extra_r)
    #the underlying parameter is number of "steps"
    numbers$n <<- unique(df$trial_motion_process_n + 1)
    numbers$contrast <<- unique(with(
        df, trial_extra_content_ccw + trial_extra_content_cw))
    numbers$fixationToOnset <<- unique(with(
        df, trial_fixationSettle + trial_motion_process_t))
    numbers$onsetToInput <<- unique(with(
        df, trial_awaitInput - trial_motion_process_t))
    numbers$onsetToTimeout <<- unique(with(
        df, trial_awaitInput - trial_motion_process_t + trial_maxResponseLatency))
    
  })


  with.db.connection(drv, dbfile, fn=function(conn) {
    df <- pull.from.db(
      conn, data.frame(loaded_from=segment.files),
      columns=c(
          "trial_extra_flanker1Angle", "trial_extra_flanker2Angle",
          "trial_extra_r", "trial_extra_min_distance", "trial_extra_nTargets",
          "trial_extra_nVisibleTargets"))
    numbers$min.flanker.angle <<-
        min(with(df, abs(trial_extra_flanker1Angle -
                         trial_extra_flanker2Angle)))
    numbers$max.flanker.angle <<-
        max(with(df, abs(trial_extra_flanker1Angle -
                         trial_extra_flanker2Angle)))
    numbers$min.distance <<-
        unique(with(df, trial_extra_min_distance * trial_extra_r))
    numbers$tested <<- chain(
        df[c("trial_extra_nTargets",
             "trial_extra_nVisibleTargets",
             "trial_extra_r")],
        unique,
        mutate(spacing = 2*pi*trial_extra_r / trial_extra_nTargets,
               radians = 2*pi/trial_extra_nTargets),
        merge(data.frame(trial_extra_nTargets=(c(12,21,15,15)),
                         trial_extra_nVisibleTargets = c(5,5,3,6),
                         selected=T),
              all.x=T))
    numbers$segment.ecc <<- unique(df$trial_extra_r)
  })

  save(numbers, calib, file=outfile)
}

run_as_command()
