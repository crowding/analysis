#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  source("db_functions.R")
  source("helper_functions.R")
  library(plyr)
  library(stringr)
  library(ptools)
  library(R.matlab)
})

renamings <- {
  c(
    abs_displacement              = "abs_displacement",
    abs_localDirectionContrast    = "abs_direction_content",
    abs_response                  = "",
    abs_response_cw               = "abs_response_cw",
    folded_displacement           = "folded_displacement",
    folded_localDirectionContrast = "folded_direction_content",
    folded_response               = "",
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
    trial_i                       = "trial.order",
    trial_motion_process_n        = "n",
    trial_motion_process_order    = "order",
    trial_motion_process_radius   = "eccentricity"
    )}

only_important_for_motion_energy <-
  c("n", "order", "wavelengthScalar", "widthScalar", "durationScalar", "dt")

rename_with_drop <- function(x, renamings) {
  chain(x,
        drop_columns(names(renamings)[renamings == ""]),
        rename(renamings[renamings != ""]))
}

drop_columns <- function(data, drop) {
  data[colnames(data)[!colnames(data) %in% drop]]
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
        mutate(abs_response_cw = abs_response > 0,
               folded_response_with_carrier = folded_response > 0),
        subset(responseInWindow == TRUE),
        rename_with_drop(renamings),
        attach_motion_energy(motion_energy_file),
        drop_columns(only_important_for_motion_energy),
        write.csv(outfile, row.names=FALSE)
        )
}

attach_motion_energy <- function(trials, motion_energy_file) {
  chain(motion_energy_file,
        readMat(fixNames=FALSE),
        .$data[,1,1],
        as.data.frame
        ) -> menergy

  trials$left.check <- 1:nrow(trials)
  menergy$right.check <- 1:nrow(menergy)

  joined <- merge(trials, menergy, type="inner",
                  on = intersect(names(trials), names(menergy)))
  if (any(aups <- duplicated(joined$left.check))) {
    warning("Ambiguous motion-energy matches?")
    joined <- joined[!dups]
  }
  if (any(missed <- is.na(joined$right.check))) {
    warning("Motion energy information not found for all trials")
  }
  joined
}

run_as_command()
