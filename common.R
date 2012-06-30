#!/usr/bin/env Rscript

##Manipulations done to normalize my data.

suppressPackageStartupMessages({
  library(ptools)
  library(plyr)
})

common <- function(infile, outfile) {
  env <- new.env(parent=.GlobalEnv)
  suppressPackageStartupMessages({
    load(infile, envir=env)
    source("common.manipulations.R")
  })
  common.manipulations(env)
  save(envir=env, list=ls(env), file=outfile)
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly = TRUE)
  do.call('common', as.list(args))
}

