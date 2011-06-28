#!/usr/bin/env Rscript

aggregate <- function(...) {
  library(gtools)
  ###aggregate several unpacked data files, and save to the named
  ###output file.
  infiles <- list(...)
  outfile <- infiles[[length(infiles)]]
  infiles[[length(infiles)]] = NULL

  envs <- lapply(infiles, load.discarding.eye.position)

  run.offset <- 0
  trial.offset <- 0
  envs <- lapply(envs, function(e) {
    run.offset <<- run.offset - min(e$runs$i, 1) + 1
    trial.offset <<- trial.offset - min(e$trials$i, 1) + 1

    e$runs$i <- e$runs$i + run.offset;
    e$trials$runs.i <- e$trials$runs.i + run.offset
    e$trials$i <- e$trials$i + trial.offset
    e$triggers$trials.i <- e$triggers$trials.i + trial.offset
    e$frame.skips$trials.i <- e$frame.skips$trials.i + trial.offset

    run.offset <<- max(e$runs$i, run.offset)
    trial.offset <<- max(e$trials$i, trial.offset)
    
    e
  })

  env.out <- new.env()
  for (i in Reduce(union, sapply(envs, ls))) {
    collection <- lapply(envs, `[[`, i)
    collection <- collection[sapply(collection, function(x) length(x[[1]])) > 0]
    env.out[[i]] <- do.call(mysmartbind, collection)
  }
  save(list=ls(env.out), file=outfile, envir=env.out)
  env.out
}

load.discarding.eye.position <- function(infile) {
  ###Load a file, discard the eye position data, and return as an
  ###environment object.
  e <- new.env()
  load(infile, e)
  e$trials$eyeData <- NULL
  e$trials$eyeData <- NULL
  e
}

mysmartbind <- function(...) 
{
  ## smartbind from gtools, but lets vectors promote their datatypes if
  ## needed.
    verbose <- FALSE
    data <- list(...)
    if (is.null(names(data))) 
        names(data) <- as.character(1:length(data))
    data <- lapply(data, function(x) {
      if (is.matrix(x) || is.data.frame(x)) x else data.frame(as.list(x))
    } )
    retval <- list()
    rowLens <- unlist(lapply(data, nrow))
    nrows <- sum(rowLens)
    rowNameList <- unlist(lapply(names(data), function(x) {
      if (rowLens[x] <= 1) x else paste(x, seq(1, rowLens[x]), sep = ".")
    } ))
    start <- 1
    for (block in data) {
        if (verbose) 
            print(block)
        end <- start + nrow(block) - 1
        for (col in colnames(block)) {
            if (!(col %in% names(retval))) {
                if (verbose) 
                  cat("Start:", start, "  End:", end, "  Column:", 
                    col, "\n", sep = "")
                if (class(block[, col]) == "factor") 
                  newclass <- "character"
                else newclass <- class(block[, col])
                retval[[col]] <- as.vector(rep(NA, nrows), mode = newclass)
            }
##            retval[[col]][start:end] <- as.vector(block[, col], 
##                mode = class(retval[[col]]))
            retval[[col]][start:end] <- as.vector(block[, col])
          }
        start <- end + 1
    }
    attr(retval, "row.names") <- rowNameList
    class(retval) <- "data.frame"
    return(retval)
}

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call(aggregate, as.list(my.args))
}
