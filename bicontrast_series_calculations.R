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

  flist <- file(outfile, 'w')
  on.exit(close(flist), add=TRUE)
  
  trials <- pull.from.sqlite(dbfile, data.frame(loaded.from=files))
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("main", as.list(args))
}
