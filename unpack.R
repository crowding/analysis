#!/usr/bin/env Rscript

library(manipulations)
library(gtools)
library(abind)

is.matlab.struct <-  function(.) {
  (sum(dim(.)[-1] != 1) <= 1) && (length(dimnames(.)[1]) > 0)
}

struct.to.data.frame <- function(.) {
  if ( is.matlab.struct(.) ) {
    ##as.data.frame will respond to a 3d array by undesirably munging
    ##the column names. But it needs at least a 2d array. with the
    ##correct column names.
    as.data.frame(array(
                aperm(., c( (1:length(dim(.)))[-1],1 )),
                dim=c( prod(dim(.)[-1]), dim(.)[1] ),
                dimnames=list(NULL, dimnames(.)[[1]])
                ))
  } else if (length(.) == 1 && is.na(.)) {
    data.frame()
  } else {
    .
  }
}

struct.list.to.one.data.frame <- function(., indexcolumn = "", sequencecolumn="") {
  dfs <- lapply(., struct.to.data.frame)
  nonempty <- sapply(dfs,nrow) != 0
  if (nchar(indexcolumn)) {
    for (i in as.integer((1:length(dfs))[nonempty])) {
      dfs[[i]][,indexcolumn] <- i
      if (nchar(sequencecolumn)) {
        dfs[[i]][,sequencecolumn] <- as.integer(seq(1, nrow(dfs[[i]])))
      }
    }
  }
  if (length(dfs[nonempty])) do.call(smartbind, dfs[nonempty]) else data.frame()
}

lists.of.structs.to.one.data.frame <- function(., indexcolumn="", sequencecolumn="") {
  dfs <- lapply(., struct.list.to.one.data.frame, indexcolumn=sequencecolumn)
  ##annoying: both smartbind and assigning the index column fail on
  ##empty data frames.
  nonempty <- sapply(dfs,nrow) != 0
  if (nchar(indexcolumn)) {
    ##annoying: prevent error assigning to empty dfs[[i]] is empty
    for (i in as.integer((seq(1, len=length(dfs)))[nonempty])) {
      dfs[[i]][,indexcolumn] <- i
      if (nchar(sequencecolumn)) {
        dfs[[i]][,sequencecolumn] <- as.integer(seq(1, nrow(dfs[[i]])))
      }
    }
  }
  if (length(dfs[nonempty])) do.call(smartbind, dfs[nonempty]) else data.frame()
}

unpack.directly <- function(df, column) {
  #unpack and left-join in one operation.
  left <- df
  left[,"column"] <- NULL
  left[,"temporary.index"] <- 1:nrow(df)
  right <- struct.list.to.one.data.frame(df[,column], 'temporary.index')
  colnames(right) <- paste(column, ".", colnames(right),sep="")
  out <- merge(left, right,
               by.x='temporary.index',
               by.y=paste(column,".","temporary.index",sep=""),
               all.x=TRUE)
  out$temporary.index <- NULL
  out[,paste(column,".","temporary.index",sep="")] <- NULL
  out[,column] <- NULL
  out
}

##Detect fields that are always either empty or singleton MATLAB
##structs; unpack them. Detect fields that can be collapsed into a
##primitive data type; collapse them.
unpack.coerce <- function(frame, coerce=TRUE, unpack=TRUE, ignoring=character()) {
  cnl <- colnames(frame)
  while (length(cnl) >= 1) {
    col <- cnl[[1]]
    cnl <- cnl[-1]
    ##
    if(coerce && !(col %in% ignoring)) {
      if (mode(frame[,col]) == "list"
          && all(lapply(frame[,col], mode) != "list")
          && all(unlist(lapply(frame[,col], length)) == 1)) {
        cat("coerce ", col, "\n")
        frame[,col] <- unlist(frame[,col], recursive=FALSE)
        if (storage.mode(frame[,col]) == "character") {
          frame[,col] <- factor(frame[,col])
        }
      }
    }
    ##
    if(unpack && !(col %in% ignoring)) {
      if (mode(frame[,col]) == "list") {
        isna <- sapply(frame[,col], function(x) length(x) == 0 || is.na(x))
        isstruct <- sapply(frame[!isna,col], function(.) is.matlab.struct(.) && prod(dim(.)[-1]) <= 1)
        if (any(isstruct) && all(isstruct)) {
          oldnames <- colnames(frame)
        cat("unpack ", col, "\n")
          frame <- unpack.directly(frame, col)
        cnl <- c(cnl, setdiff(colnames(frame), oldnames))
        }
      }
    }
  }
  frame
}

unpack <- function(infile, outfile) {
  ##Unpack the file's data hierarchy into a set of joinable tables...
  load(infile); 
  runs <- struct.list.to.one.data.frame(data$data)
  rm(data)
  runs$i <- as.integer(seq(1, len=nrow(runs)))
  trials <- lists.of.structs.to.one.data.frame(runs$trials, "runs.i", "run.seq")
  runs$trials <- NULL

  ##the triggers are a list of grouped structs...
  trials$i <- as.integer(seq(1, len=nrow(trials)))
  triggers <- struct.list.to.one.data.frame(trials$triggers, "trials.i", "trigger.seq")

  trials$triggers <- NULL

  frame.skips <- struct.list.to.one.data.frame(trials$frame.skips, "trials.i", "skip.seq")
  trials$frame.skips <- NULL

  trials <- unpack.coerce(trials, ignoring="params")
  triggers <- unpack.coerce(triggers)
  runs <- unpack.coerce(runs, ignoring=c("beforeRun.params","afterRun.params"))
  frame.skips <- unpack.coerce(frame.skips)

  save("runs", "trials", "triggers", "frame.skips", file=outfile)
}

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call(unpack, as.list(my.args))
}
