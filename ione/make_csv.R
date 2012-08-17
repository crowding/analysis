#import files from the 
#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  source("db_functions.R")
  source("good_subjects.R")
  library(plyr)
})

main <- function(flist, dbfile, outfile) {
  files <- str_trim(reaLines(flist))
  trials <- pull.from.sqlite(dbfile, data.frame(loaded_from = files))
  trials <- subset(trials, responseInWindow=TRUE)
  trials <- rename(trials, renaming[names(renaming) %in% colnames(trials)])
  write.csv(trials, outfile)
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("main", as.list(args))
}
