#!/usr/bin/Rscript

require(R.matlab)

args = commandArgs(trailingOnly=TRUE)

mfilein <- args[[1]]
rfileout <- args[[2]]

data <- readMat(mfilein)
save(data, file=rfileout)
