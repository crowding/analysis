#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ptools)
  source("db_functions.R")
})

renamings <- c(
                 trial_motion_process_radius   = "eccentricity",
                 abs_localDirectionContrast    = "abs_direction_content",
                 abs_displacement              = "abs_displacement",
                 target_spacing                = "target_spacing",
                 trial_extra_nTargets          = "target_number_all",
                 trial_extra_nVisibleTargets   = "target_number_shown",
#is side necessary if we track phase?
                 trial_extra_side              = "side",
                 abs_response_cw               = "abs_response_cw",
                 trial_extra_flanker1Phase     = "flanker1phase",
                 trial_extra_flanker2Phase     = "flanker2phase",
                 trial_extra_tf                = "tf",
                 trial_extra_wavelengthScalar  = "wavelengthScalar",
                 trial_extra_dt                = "dt",
                 trial_extra_phase             = "phase",
                 trial_extra_widthScalar       = "widthScalar",
                 trial_extra_durationScalar    = "durationScalar",
                 trial_motion_process_order    = "order",
                 trial_motion_process_n        = "n"
                 )

make_further_unique <- function(trials){
  mutate(trials,
         phase = ifelse( is.na(flanker1phase) | is.na(flanker2phase),
                         NA, phase))
}

main <- function(dbfile, outfile, ...) {
  listing_files <- c(...)
  filenames <- chain(listing_files, readLines, unique)
  drv <- SQLite()
  data <- pull.from.sqlite(dbfile,
                           columns.to.pull = names(renamings),
                           data.frame(loaded.from=filenames))
  chain(data,
        rename(renamings),
        make.further.unique,
        unique) -> data

  save(data, file=outfile)
}


