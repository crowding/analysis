#!/usr/bin/env Rscript
##Download and install necessary packages.

main <- function(install.dir, ...) {
  packages.i.need <- do.call(Reduce, list(union, c("devtools", lapply(c(...), getLines))))
  packages.i.github <- c(ptools="crowding/ptools")
  packages.i.have <- .packages(all.available=TRUE)

  if (length(setdiff(packages.i.need, packages.i.have)) > 0) {
    install.packages(setdiff(packages.i.need, packages.i.have),
                     repos = c(
                       CRAN = "http://cran.r-project.org/",
                       RForge="http://R-Forge.R-project.org/"
                       ),
                     lib=install.dir)
  }

  for (i in setdiff(names(packages.i.github), packages.i.have)) {
    library(devtools)
    install.github(packages.i.github[[i]], lib=install.dir)
  }
}

getLines <- function(file) {
  lines <- readLines(file)
  gsub("^\\s+|\\s+$", "", lines)
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("main", as.list(args))
}
