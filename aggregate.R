#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  source("programming.R")
  library(gtools)
  library(ptools)
})

aggregate <- function(...) {
  ###aggregate several unpacked data files, and save to the named
  ###output file.
  infiles <- unlist(list(...))
  if ("--drop.triggers" %in% infiles) {
    drop.triggers <- TRUE
    infiles <- infiles[-which(infiles=="--drop.triggers")]
  } else {
    drop.triggers <- FALSE
  }

  outfile <- infiles[[length(infiles)]]
  infiles <- infiles[-length(infiles)]

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

    #While old data percolate through the system.... with the new logfile
    #parser this will become irrelevant as frame skips are now listed
    #in the trigers table.
    if ("frame.skips" %in% ls(e)) rm("frame.skips", envir=e)

    if (drop.triggers) rm("triggers", envir=e)

    run.offset <<- max(e$runs$i, run.offset)
    trial.offset <<- max(e$trials$i, trial.offset)

    e
  })

  env.out <- new.env()
  for (i in Reduce(union, sapply(envs, ls))) {
    collection <- lapply(envs, `[[`, i)
    collection <- collection[!vapply(collection, empty, 0)]
    collection <- collection[sapply(collection, function(x) length(x[[1]])) > 0]
    if(length(collection) > 0) {
      env.out[[i]] <- do.call("mysmartbind", collection)
    }
  }
  save(list=ls(env.out), file=outfile, envir=env.out)
  env.out
}

kill.list.cols <- function(df) {
  chain(  df
       , lapply(mode)
       , .[.=="list"]
       , names
       , setdiff(colnames(df),.)
       , df[,.]
       )
}

load.discarding.eye.position <- function(infile) {
  ###Load a file, discard the eye position data, and return as an
  ###environment object.
  e <- new.env()
  print(infile)
  load(infile, e)
  e$trials$eyeData <- NULL
  e$trials$trial.eyeData <- NULL

  if (!"subject" %in% colnames(e$runs)) {
    e$runs$subject <- e$runs$beforeRun.params.subject
  }
  if (! "source.file" %in% colnames(e$runs)) {
    e$runs$source.file <- e$runs$beforeRun.params.logfile
  }

  ##fukka any column that's still in list mode.
  e$runs <- kill.list.cols(e$runs)
  e$trials <- kill.list.cols(e$trials)
  e$triggers <- kill.list.cols(e$triggers)

  if (!"subject" %in% colnames(e$runs)) {
    stop("dammit!")
  }

  if (!"source.file" %in% colnames(e$runs)) {
    stop("what the hell!")
  }
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
                else newclass <- mode(block[, col])
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
  do.call("aggregate", as.list(my.args))
}
