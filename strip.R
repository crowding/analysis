#/usr/bin/env Rscript

strip <- function(infile, outfile) {
  env <- new.env(parent=.GlobalEnv)
  load(infile, envir=env)
  env$trials$eyeData <- NULL
  env$trials$trial.eyeData <- NULL
  save(envir=env, list=ls(env), file=outfile)
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly = TRUE)
  do.call('strip', as.list(args))
}

