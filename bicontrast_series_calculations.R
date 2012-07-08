suppressPackageStartupMessages({
	library(ggplot2)
        library(plyr)
        library(ptools)
        library(stringr)
        library(psyphy)
        source("db_functions.R")
        source("graphics_functions.R")
        source("data_functions.R")
})

`%-%` <- setdiff
`%+%` <- union

main <- function(flist, dbfile, outfile) {

  files <- str_trim(readLines(flist))

  outputs <- file(outfile, 'w')
  on.exit(close(outputs), add=TRUE)

  trials <- pull.from.sqlite(dbfile, data.frame(loaded.from=files))
  
  threshes <- measure_thresholds(trials, sims=0, use_folded=TRUE, abs_bias=TRUE)
  athreshes <- measure_thresholds(trials, sims=0, use_folded=FALSE, abs_bias=FALSE)
  threshes <- mutate(threshes, contrast = folded_content_with + folded_content_against)
  athreshes <- mutate(athreshes, contrast = trial_extra_content_cw + trial_extra_content_ccw)

  pdf_file <- replace_extension(outfile, "pdf")
  writeLines(pdf_file, outputs)
  pdf(pdf_file, onefile=TRUE)
  on.exit(dev.off(), add=TRUE)
  
  (ggplot(subset(athreshes, abs(yint) < 100))
   + aes(trial_extra_content_ccw - trial_extra_content_cw, yint, color=contrast, group=contrast, ymin = yint+yint.sd, ymax = yint - yint.sd)
   + geom_point()
   + scale_color_gradientn(name="Luminance Contrast\n(CW + CCW)"
                          , breaks=seq(0.1,0.5, by=0.1)
                          , colours=c(low="#0088FF", mid="#444444", high="#FFAA00"))
   + scale_x_continuous(name="Opponent Carrier Motion (CW - CCW)")
   + scale_y_continuous(name="Bias (CW responses to stationary envelope, log-odds)", breaks=seq(-4,4,2))
   + facet_grid(subject ~ target_spacing)
   + geom_line()
   + geom_errorbar(width=0)
   + coord_cartesian(ylim=c(-6,6))
   + geom_hline(y=0)
   + geom_hline(y=0)
   + theme_bw()
   ) -> bias_plot
  
  ## (ggplot(subset(threshes, abs(yint) < 100))
  ##  + aes(contrast, slope, color=
  ##        , ymin = slope+slope.sd, ymax = slope - slope.sd, group=trial_extra_content_cw - trial_extra_content_ccw)
  ##  + geom_point()
  ##  + facet_grid(subject ~ target_spacing, scales="free")
  ##  + geom_errorbar(width=0)
  ##  + theme_bw()
  ##  + scale_y_continuous("Sensitivity to envelope motion", limits=c(-10, 50))
  ##  + scale_x_continuous("Luminance Contrast\n(CW + CCW)")
  ##  + scale_color_gradientn(name="Carrier Motion (CW - CCW)"
  ##                          , colours=c(low="#0088FF", mid="#444444", high="#FFAA00"))
  ##  + geom_hline(y=0)
  ##  ) -> slope_plot


    (ggplot(subset(threshes, abs(yint) < 100))
   + aes(contrast, slope, color=folded_content_with - folded_content_against
         , ymin = slope+slope.sd, ymax = slope - slope.sd, group=folded_content_with - folded_content_against)
   + geom_point()
   + facet_grid(subject ~ target_spacing, scales="free")
   + geom_errorbar(width=0)
   + theme_bw()
   + scale_y_continuous("Sensitivity to envelope motion", limits=c(-10, 50))
   + scale_x_continuous("Luminance Contrast\n(incongruent + congruent)")
   + scale_color_gradientn(name="Carrier Motion\n(incongruent - congruent)"
                           , colours=c(low="#0088FF", mid="#444444", high="#FFAA00"))
   + geom_hline(y=0)
   ) -> slope_plot

  print(bias_plot)
  print(slope_plot)

   #Quick gloss!
   #Bias seems to be controlled by the difference betwen congruent and incongruent carriers.
   #Sensitivity (to envelope motion) seems to be controlled by the contrast (except that ML is more sensitive to perfectly counterphase motion, but this is a motion energy effect.
  
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("main", as.list(args))
}
