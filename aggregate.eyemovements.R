suppressPackageStartupMessages({
  source("common.manipulations.R")
  source("programming.R")
  library(plyr)
})

aggregate.eyemovements <- function(...) {
  filenames <- c(...)
  output <- filenames[length(filenames)]
  filenames <- filenames[-length(filenames)]
  loaded <- lapply(filenames, load.as.list)
  ##instigate some offsets for trial id numbers
  i.offset <- 0
  loaded <- llply(loaded, function(d) {
    omin <- min(c(d$trials$i,0))
    d$trials$i <- d$trials$i + i.offset - omin + 1
    d$traces$i <- d$traces$i + i.offset - omin + 1
    i.offset <<- max(c(i.offset,d$trials$i))
    rownames(d$traces) <- NULL
    rownames(d$trials) <- NULL
    d
  })
  names(loaded) <- NULL
  trials <- do.call(rbind, lapply(loaded, fn(.$trials)))
  traces <- do.call(rbind, llply(loaded, fn(.$traces)))
  save(trials, traces, file=output)
}

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call("aggregate.eyemovements", as.list(my.args))
}
