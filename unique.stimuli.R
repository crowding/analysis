##

suppressPackageStartupMessages({
  library(plyr)
  library(R.matlab)
})

stimulus.pool <- function(...) {
  # basically we want a list of all the unique stimuli used in the experiment.
  # this will be fed to matlab for use in a motion energy calculation.
  my.args <- list(...)
  output.file <- my.args[[length(my.args)]]
  my.args[length(my.args)] <- NULL

  #load each file and get the unique stimuli.
  stimuli <- unique(ldply(my.args, extract.stimuli))
  writeMat(output.file, stimuli=stimuli)
}

extract.stimuli <- function(filename) {
  print(filename)
  load(filename)
  
  #and just extract the data from trials that you need.
  unique(stimulus.description(subset(trials, trial.version__.function == "ConcentricTrial")))
}

stimulus.description <- function(trials) {
  ##just select all columns that are sufficient to describe the
  ##stimulus (as it was varied in this experiment)

  ##Fix the phase. Make sure it's in the same direction as the motion direction......argh.
  ##Luckily, since I used vertically symmetric occluders, 'left' and 'right' should be sufficient.
  ##We alraedy know that globalDirection is the same sign as
  ##dphase... so phase moves in the same direction as globalDirection.

  ##Ima stab someone if phase of the 120 Hz screen refresh is
  ##important, so I'll leave that out. Even though I have that
  ##information (in principle.)
  
  trials$trial.extra.phase = trials$trial.extra.phase * trials$trial.extra.globalDirection

  #phase 
  trials$trial.extra.phase[trials$visibilityCondition == 'full'] <- 0

  #as far as I know, all of these fully determine the stimulus appearance.
  #Select the ones that are present on out dataset.
  columns <- c("trial.motion.process.radius",
               "trial.extra.side",
               "trial.extra.nVisibleTargets",
               "trial.extra.flankerPhase",
               "abs.displacement",
               "abs.localDirectionContrast",
               "trial.extra.tf",
               "trial.extra.wavelengthScalar",
               "trial.extra.dt",
               "trial.extra.phase",
               "trial.extra.widthScalar",
               "trial.extra.durationScalar",
               "trial.extra.nTargets",
               "trial.motion.process.order",
               "trial.motion.process.n",
               "visibilityCondition")

  trials[,columns[! (columns %in% colnames(trials))]] <- NA
  
  stimuli <- subset(trials
                    , select=columns)
}

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call(stimulus.pool, as.list(my.args))
}
