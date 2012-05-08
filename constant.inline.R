library(plyr)
library(ggplot2)

source("common.manipulations.R")
source("programming.R")

my.args <- list()

load("Constant.Rdata")
calibration <- runs[[1,'afterRun.params']][['cal',1,1]]
screen.distance <- calibration[["distance",1,1]]
display.resolution <- calibration[["rect",1,1]][c(3,4)]
display.size <- display.resolution*calibration[["spacing",1,1]]
black.luminance <- calibration[["calibration",1,1]][["stage2",1,1]][["readings",1,1]][c(1)]
white.luminance <- calibration[["calibration",1,1]][["stage2",1,1]][["readings",1,1]][c(256)]
refresh.rate <- round(1/calibration[["interval",1,1]])
sample.rate <- runs[[1,'beforeRun.params']][['eyelinkSettings',1,1]][['sample.rate',1,1]]
