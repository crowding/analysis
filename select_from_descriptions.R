#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(plyr)
  library(ptools)
})

main <- function(scriptfile, infile, outfile) {
  ##the "scriptfile" defines a function "is.good.session"
  source(scriptfile)
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
