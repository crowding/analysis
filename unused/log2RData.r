library(testthat)
library(stringr)
library(plyr)

#man, this is ad hoc, shoulda gone with flex/bison. would probably be a lot faster.....

toEntry <- function(line) {
  s <- str_split(line, ' *= *', 2)[[1]]
  address <- s[1]
  data <- s[2]
  if (str_detect(address,fixed("(")) || str_detect(address,fixed("{"))) {
    return(list())
  }
  numbers <- toMatrix(data)
  structure(list(numbers), names=address)
}

toMatrix <- function(data) {
  if (str_detect(data, fixed('('))) {
    ##need sprintf, uint8, zeros, what else? Are any cell variables relevant? Probably not.
    arg <- str_sub(str_extract(data, '\\(.*\\)'), 2, -2)
    name <- str_sub(str_extract(data, '.*\\('), 1, -2)
    switch(name
           , sprintf=return(sprintf(str_sub(arg,2,-2)))
           , uint8=data <- arg
           , uint16=data <- arg
           , {
             return(list())
           }
           )
  }
  rows = str_split(data, fixed(';'))
  words <- strsplit(rows[[1]], '\\[| |\\]')
  numbers <- sapply(words, function(x) {
    x <- x[x != ""]
    as.numeric(x)
  }, simplify=FALSE)
  numbers <- do.call(rbind, numbers)
}

use.stack.factor <- 10

data.frame.stacker <- function(stack, ...,
                               stack.factor = use.stack.factor
                               )
  {
    stack <- c(stack, list(...))
    while(length(stack) >= stack.factor) {
      ix <- (length(stack)-stack.factor+1):length(stack)
      if ( sum(sapply(stack[ix[-1]], nrow ) ) >= nrow(stack[[ix[1]]]) ) {
        stacked <- do.call(smartbind, stack[ix])
        stack[ix] <- NULL
        stack <- c(stack, list(stacked))
      } else {
        break
      }
        }
    stack
  }

cbind.stacker <- function(stack, ...,
                               stack.factor = use.stack.factor
                               )
  {
    stack <- c(stack, list(...))
    while(length(stack) >= stack.factor) {
      ix <- (length(stack)-stack.factor+1):length(stack)
      if ( sum(sapply(stack[ix[-1]], ncol ) ) >= ncol(stack[[ix[1]]]) ) {
        stacked <- do.call(cbind, stack[ix])
        stack[ix] <- NULL
        stack <- c(stack, list(stacked))
      } else {
        break
      }
        }
    stack
  }

test.cbind.stacker <- function() {
  test <- list()
  for (i in 1:100) {test <- cbind.stacker(test, array(1:6 + i, c(3, 2)))}
  do.call(cbind, test)
}

list.to.single.row <- function(x) {
  data.frame(rbind(enlist(x)))
}

test.data.frame.stacker <- function() {
  testrow <- function(x)list.to.single.row(list(a = 1, b = runif(1), c = runif(2), d = as.character(runif(1))))
  stack <- list()
  for (i in 1:100) {
    stack <- data.frame.stacker(stack, testrow())
  }
  do.call(smartbind, stack)
}

list.stacker <- function(stack, ..., stack.factor = use.stack.factor) {
  stack <- c(stack, list(...))
  while(length(stack) >= stack.factor) {
    ix <- (length(stack)-stack.factor+1):length(stack)
    if ( sum(sapply(stack[ix[-1]], length ) ) >= length(stack[[ix[1]]]) ) {
      stacked <- do.call(c, stack[ix])
      stack[ix] <- NULL
      stack <- c(stack, list(stacked))
    } else {
      break
    }
  }
  stack
}

test.list.stacker <- function() {
  stack <- list()
  for (l in letters) {
    entry <- structure(list(switch(sample(1:3, 1)
           , runif(1)
           , paste(letters[sample(1:26, 5)], collapse="")
           , runif(2)
           )), names=l)
    stack <- list.stacker(stack, entry)
  }
  do.call(c, stack)
}

log2RData <- function (filename.in, filename.out) {
  #read logfiles directly into R data frames.
  require(stringr)

  run.stack = list()
  trial.stack = list()
  frame.skip.stack <- list()
  trigger.stack = list()
  eye.stack = list()

  #one line at a time (we won't worry about legacy data with multilines...
  #first two words?
  run.index <- 0
  trial.index <- 0
  event.index <- 0
  trigger.index <- 0

  current.run <- list()
  current.trial <- list()
  
  beginRun <- function(line) {
    if (length(current.run) > 0) {
      current.run <<- do.call(`c`, c(current.run, list(i=run.index)))
      run.stack <<- data.frame.stacker(run.stack, list.to.single.row(current.run))
      current.run <<- list()
      run.index <<- run.index + 1
    }
    current.run <<- list()
    print(run.index)
  }

  enlist <- function(x) lapply(x, function(x)if(length(x) > 1) list(x) else x)
  
  beginTrial <- function(line) {
    if (length(current.trial) > 0) {
      current.trial <<- do.call(`c`, c(current.trial,
                                       list(i = trial.index, run.i = run.index,
                                            eyeData=do.call(cbind, eye.stack))))
      eye.stack <<- list()
      trial.stack <<- data.frame.stacker(trial.stack, list.to.single.row(current.trial))
      current.trial <<- list()
      trial.index <<- trial.index + 1
      if (trial.index >= 20) {
      }
    }
    current.trial <<- list()
    print(trial.index)
  }

  trigger <- function(line) {
    trigger.index <<- trigger.index + 1
    ##fairly ad-hoc, eh...
    split <- str_match(line, "([^ ]*) *([^ ]*) *(.*)")[,-1]
    
    rest <- split[[3]]
    pieces <- str_match_all(rest, "[^;[:blank:]]*=([^;]|\\[[^\\]]*\\]|\\([^\\)]*\\))*")[[1]][,1]
    data <- sapply(pieces, toEntry)

    trigger.stack <- data.frame.stacker(trigger.stack, c(data, list.to.single.row(list(trials.i=trial.index, name=split[[2]], message=split[[1]]))))
    
    #do-nothing yet
  }

  eyeData <- function(line) {
    data <- toMatrix(str_match(line, "EYE_DATA (.*)")[[1,2]])
    eye.stack <<- cbind.stacker(eye.stack, data)
  }

  trialData <- function(line) {
    current.trial <<- list.stacker(current.trial, toEntry(line))
  }

  runData <- function(line) {
    current.run <<- list.stacker(current.run, toEntry(line))
  }

  skip <- function(line) {
    #really do nothing
  }

  handlers <-  c(  "BEGIN EXPERIMENT_RUN" = beginRun
                 , "BEGIN TRIAL"          = beginTrial
                 , "TRIGGER"              = trigger
                 , "EYE_DATA"             = eyeData
                 , "trial"                = trialData
                 , "beforeRun"            = runData
                 ,                          skip
                 )
               
  conn <- gzfile(filename.in, 'r')
  on.exit(close(conn))
  
  block = 500000;
  repeat {
    lines <- readLines(conn,block)
    matches <- match.strs(lines, names(handlers), no.match=length(handlers))
    mapply(function(x,y)x(y), handlers[matches], lines)
    if (length(lines) == 0) break
  }

  ##and finish...
  beginTrial("")
  beginRun("")
  trials <- do.call(smartbind, trial.stack)
  runs <- do.call(smartbind, run.stack)
  triggers <- do.call(smartbind, trigger.stack)

  ##and save...
  save(file=filename.out, list=c("trials", "runs", "triggers"))
}

match.strs <- function(candidates, targets, no.match = NA) {
  matches <- numeric(length(candidates)) + no.match
  for (i in length(targets):1) {
    matches[substr(candidates, 1, nchar(targets[i])) == targets[i]] <- i
  }
  matches
}

test.match.strs <- function() {
  expect_equal(match.strs("foo", c("foo", "bar", "baz")), 1)
  expect_equal(match.strs("bof", c("foo", "bar", "baz")), as.numeric(NA))
  expect_equal(match.strs(c("foo", "bar", "foo"), c("foo", "bar", "baz")), c(1,2,1))
  expect_equal(match.strs(c("foo", "bar", "bof"), c("foo", "bar", "baz")), c(1,2,NA))
  expect_equal(match.strs(c("foo", "bar", "bof"), c("foo", "bar", "baz"), no.match=5), c(1,2,5))
  expect_equal(match.strs(c("bartholemew"), c("foo", "bar", "bof")), c(2))
}


if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call("log2RData", as.list(my.args))
}
