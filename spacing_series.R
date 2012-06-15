#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(plyr)
  library(ptools)
})

is.good.session <- function(desc) {
  ##For the spacing series plot, nominate sessions that have at least
  ##4 elements in nTargets, and only 5 elements in nTargets.
  spacing <- sapply(desc, mkchain(colnames, all.equal("trial.extra.nTargets"), isTRUE))
  chain(desc, sapply(nrow), .<2, `|`(spacing), all)
}

main <- function(infile, outfile) {
  env <- new.env(parent=globalenv())
  load(infile, envir=env)
  conn <- file(outfile, 'w')
  on.exit(close(conn), add=TRUE)
  mapply(env$descriptions, names(env$descriptions), FUN=function(desc, name) {
    if (is.good.session(desc)) {
      cat(name, "\n", file=conn)
    }
  })
  invisible(NULL)
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("main", as.list(args))
}
