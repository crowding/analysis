#!/bin/env Rscript
library(ptools)

drop_columns <- function(data, drop) {
  data[colnames(data)[!colnames(data) %in% drop]]
}

replace_extension <- function(filename, new_extension, append="") {
  sub(  "((.)\\.[^.]*|)$"
      , paste("\\2", append, ".", new_extension, sep="")
      , filename)
}
