#!/bin/env Rscript
library(ptools)

replace_extension <- function(filename, new_extension) {
  sub(  "((.)\\.[^.]*|)$"
      , paste("\\2.", new_extension, sep="")
      , filename)
}
