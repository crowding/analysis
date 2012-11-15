#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ptools)
  library(plyr)
  library(R.matlab)
  library(stringr)
  source("db_functions.R")
})

renamings <- c(
               trial_motion_process_radius   = "eccentricity",
               abs_localDirectionContrast    = "direction_content",
               trial_extra_content_ccw       = "content_ccw",
               trial_extra_content_cw        = "content_cw",
               abs_displacement              = "abs_displacement",
               trial_extra_nTargets          = "target_number_all",
               trial_extra_nVisibleTargets   = "target_number_shown",
               #is side necessary if we track phase?
               trial_extra_side              = "side",
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

main <- function(dbfile, outfile, matfile, ...) {
  listing_files <- c(...)

  filenames <- chain(listing_files, lapply(readLines), unlist, str_trim, unique)
  drv <- SQLite()
  data <- pull.from.sqlite(dbfile,
                           columns = names(renamings),
                           data.frame(loaded.from=filenames))
  chain(data,
        rename(renamings),
        make_further_unique,
        unique) -> data

  data_without_phase <- data
  data_without_phase[c("phase", "flanker1phase", "flanker2phase", "side")] <- list()
  data_without_phase <- unique(data_without_phase)

  save(data, data_without_phase, file=outfile)
  writeMat(con=matfile, data=data, data_without_phase = data_without_phase)

}

run_as_command()
