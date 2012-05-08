#!/usr/bin/Rscript

require(foreign)

args = commandArgs(trailingOnly=TRUE)

mfilein <- args[[1]]
rfileout <- args[[2]]

data <- read.octave(mfilein)
save(data, file=rfileout)
