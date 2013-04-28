#here we compute some aggregate numbers about the stimuli from the database.

#what is the eccentricity of the stimuli?
library(plyr)
library(stringr)
source("db_functions.R")
load("descriptions/descriptions.RData")
drv <- SQLite()
dbfile <- "discrimination.sqlite"

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

  numbers$results.eccentricity <- unique(pull.from.sqlite(
    dbfile,
    data.frame(loaded_from=results.files),
    columns="trial_motion_process_radius")[[1]])

  with.db.connection(drv, dbfile, fn=function(conn) {
    df <- pull.from.db(
      conn, data.frame(loaded_from=segment.files),
      columns=c("trial_extra_flanker1Angle", "trial_extra_flanker2Angle",
        "trial_extra_r", "trial_extra_min_distance", "trial_extra_nTargets",
        "trial_extra_nVisibleTargets"))
    numbers$min.flanker.angle <<- min(with(df, abs(trial_extra_flanker1Angle - trial_extra_flanker2Angle)))
    numbers$max.flanker.angle <<- max(with(df, abs(trial_extra_flanker1Angle - trial_extra_flanker2Angle)))
    numbers$min.distance <<- unique(with(df, trial_extra_min_distance * trial_extra_r))
    numbers$tested <<- chain(df[c("trial_extra_nTargets",
                                  "trial_extra_nVisibleTargets",
                                  "trial_extra_r")],
                             unique,
                             mutate(spacing = 2*pi*trial_extra_r / trial_extra_nTargets,
                                    radians = 2*pi/trial_extra_nTargets),
                             merge(data.frame(trial_extra_nTargets=(c(12,21,15,15)),
                                              trial_extra_nVisibleTargets = c(5,5,3,6),
                                              selected=T),
                                   all.x=T))})
  numbers$segment.ecc <<- unique(df$trial_extra_r)

  save(numbers, file=outfile)
}

run_as_command()
