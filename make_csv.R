#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  source("db_functions.R")
  source("helper_functions.R")
  library(plyr)
  library(stringr)
  library(ptools)
  library(R.utils)
  library(R.matlab)
})

renamings <- {
  c(
    abs_displacement              = "abs_displacement",
    abs_localDirectionContrast    = "abs_direction_content",
    abs_response                  = "abs_response_cw",
    folded_displacement           = "folded_displacement",
    folded_localDirectionContrast = "folded_direction_content",
    folded_response               = "folded_response_with_carrier",
    loaded_from                   = "filename",
    responseInWindow              = "",
    subject                       = "subject",
    target_spacing                = "target_spacing",
    trial_extra_content_ccw       = "content_ccw",
    trial_extra_content_cw        = "content_cw",
    trial_extra_dt                = "dt",
    trial_extra_durationScalar    = "durationScalar",
    trial_extra_nTargets          = "target_number_all",
    trial_extra_nVisibleTargets   = "target_number_shown",
    trial_extra_phase             = "phase",
    trial_extra_side              = "side",
    trial_extra_tf                = "tf",
    trial_extra_wavelengthScalar  = "wavelengthScalar",
    trial_extra_widthScalar       = "widthScalar",
    trial_i                       = "trial_order",
    trial_motion_process_n        = "n",
    trial_motion_process_order    = "order",
    trial_motion_process_radius   = "eccentricity"
    )}

only_important_for_motion_energy <-
  c("n", "order", "wavelengthScalar", "widthScalar", "durationScalar", "dt", "phase")

rename_with_drop <- function(x, renamings) {
  chain (x,
        drop_columns(names(renamings)[renamings == ""]),
        rename(renamings[renamings != ""]))
}

main <- function(flist="collections/spacing_series.list",
                 dbfile="discrimination.sqlite",
                 outfile="collections/spacing_series_trials.csv",
                 motion_energy_file="motion_energy.mat") {
  chain(flist,
        readLines,
        str_trim,
        data.frame(loaded_from=.),
        pull.from.sqlite(dbfile = dbfile, columns = names(renamings)),
        subset(responseInWindow == TRUE),
        rename_with_drop(renamings),
        mutate(
          target_number_shown = ifelse(
            is.na(target_number_shown),
            target_number_all, target_number_shown)),
        attach_motion_energy(motion_energy_file),
        drop_columns(only_important_for_motion_energy),
        write.csv(outfile, row.names=FALSE)
        )
}

attach_motion_energy <- function(trials, motion_energy_file) {
  chain(motion_energy_file,
        readMat(fixNames=FALSE),
        .$data[,1,1],
        as.data.frame,
        mutate(
          target_number_shown = ifelse(
            is.na(target_number_shown),
            target_number_all, target_number_shown))
        ) -> menergy

  trials$left.check <- 1:nrow(trials)
  menergy$right.check <- 1:nrow(menergy)

  joined <- merge(trials, menergy, type="inner",
                  by = intersect(names(trials), names(menergy)), all.x=TRUE
                  )
  if (any(dups <- duplicated(joined$left.check))) {
    #getting a lot of duplicated matches, for
    #some reason?
    warning("Multiple motion-energy matches")
    joined <- joined[!dups,]
  }
  if (any(missed <- is.na(joined$right.check))) {
    stop("Motion energy information not found for all trials")
  }
  drop_columns(joined, c("left.check", "right.check"))
}

run_as_command()
