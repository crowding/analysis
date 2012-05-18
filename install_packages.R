#!/usr/bin/env Rscript
##if these packages are not installed, install them in this directory

#the argument
name = commandArgs(trailingOnly=TRUE)[[1]]

#install.dir <- file.path(getwd(), "Rlibs")
install.dir = dirname(name)

packages.i.compile <- c("ptools")

packages.i.need <- c("plyr", "ggplot2", "stringr", "arm", "numDeriv", "Matrix"
                     , "psyphy", "boot", "utils", "lmtest", "RSQLite", "signal"
                     , "reshape", "gmodels", "ellipse", "gridExtra", "testthat"
                     , "binom", "R.matlab", "gtools", "abind")

packages.i.have <- .packages(all.available=TRUE)

if (length(setdiff(packages.i.need, packages.i.have)) > 0)
  install.packages(setdiff(packages.i.need, packages.i.have),
                   repos="http://cran.r-project.org/",
                   lib=install.dir)

for (i in setdiff(packages.i.compile, packages.i.have)) {
  check(i)
  install(i, lib=install.dir)
}

#if an output file is given, touch that file.
close(file(name, "a"))
