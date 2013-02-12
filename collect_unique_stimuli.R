#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(ptools)
  library(plyr)
  library(R.matlab)
  library(stringr)
  source("db_functions.R")
  source("helper_functions.R")
})

renamings <- c(
               trial_motion_process_radius   = "eccentricity",
               abs_localDirectionContrast    = "abs_direction_content",
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

phase_related_columns <- c("phase", "flanker1phase", "flanker2phase", "side")

make_further_unique <- function(trials) {
  mutate(trials,
         phase = ifelse( is.na(flanker1phase) | is.na(flanker2phase),
                         NA, phase))
}

expand <- function(trials) {
  #In addition to all unique trials, get a sampled grid for motion energy
  #calculating.
  chain(trials,
        subset((( target_number_shown == target_number_all)
                | is.na(target_number_shown))
               & ( wavelengthScalar == 0.075 )
               & ( dt == .1 )
               & ( eccentricity - 20/3 < 0.01 )
               & (abs(content_cw + content_ccw - 0.5) < 0.01)
               )) -> eligible

  #we keep most columns with content because content is really
  #content_cw plus content_ccw
  contents <- chain(eligible,
                    .[!duplicated(.[c("content_cw", "content_ccw")]),],
                    drop_columns(c("target_number_all", "abs_displacement")),
                    unique)

  spacings <- chain(eligible,
                    subset(select="target_number_all"),
                    unique)

  #pick some displacment values...
  displacements <-
    chain(eligible,
          ddply("abs_direction_content",
                summarize,
                range = diff(range(abs_displacement)),
                count = length(unique(abs_displacement)),
                spacing = min(diff(sort(unique(abs_displacement))))),
          subset(spacing == max(spacing)),
          subset(range == max(range)),
          subset(count == max(count)),
          .[1,],
          match_df(eligible, .),
          subset(select=c('abs_displacement'))
        )

  #this gives us a 3d grid to calculate motion energy over...
  grid <- chain(contents,
                merge(spacings),
                merge(displacements),
                mutate(grid=TRUE))

  joint <- chain(trials,
                 merge(grid, all.x=TRUE, all.y=TRUE),
                 mutate(grid=ifelse(is.na(grid), FALSE, grid)),
                 mutate(grid=as.numeric(grid)))

  joint
}

main <- function(dbfile="discrimination.sqlite",
                 outfile="unique_stimuli.RData",
                 matfile="unique_stimuli.mat", ...) {
  if (missing(...)) {
    listing_files <- Sys.glob("collections/*_series.list")
  } else {
    listing_files <- c(...)
  }

  filenames <- chain(listing_files, lapply(readLines), unlist, str_trim, unique)
  drv <- SQLite()
  data <- pull.from.sqlite(dbfile,
                           columns = names(renamings),
                           data.frame(loaded.from=filenames))
  chain(data,
        rename(renamings),
        make_further_unique,
        unique
        ) -> data

  chain(data,
        drop_columns(phase_related_columns),
        expand,
        unique
        ) -> data_without_phase

  save(data, data_without_phase, file=outfile)
  writeMat(con = matfile, data = data, data_without_phase = data_without_phase)
}

run_as_command()
