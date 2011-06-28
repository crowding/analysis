append <- function(filename, what) {
  f <- file(filename, 'a')
  tryCatch(finally=close(f), {
    writeLines(what, f)
  })
}

print.to.pdf <- function(plot,
                         file=outfilename(paste(".",deparse(substitute(plot)),".pdf",sep="")), ...) {
  pdf(file=file, ...)
  tryCatch(finally=dev.off(), {
    append(prodfile, file)
    print(plot)
  })
}

print.and.pdf <- function(plot,
                          file=outfilename(paste(".",deparse(substitute(plot)),".pdf",sep="")),
                          ...) {
  print(plot)
  print.to.pdf(plot, file=file, ...)
}


outfilename <- function(what) {
  sub('\\..*$', what,prodfile)
}

clearfile <- function(filename, what) {
  f <- file(filename, 'w')
  tryCatch(finally=close(f), {
    seek(f, 0)
    truncate(f)
  })
}

what.varies <- function(runs) {
  zip <- function(l) { ##like python zip
    lapply(apply(do.call(rbind, l), 2, list), unlist, recursive=FALSE)
  }

  whatChanges <- function(randomizer) {
    whichQuestParamsChange <- which(lapply(lapply(
      zip(randomizer[["values",1,1]]),
         unique), length) > 1)
    changingFields <- sapply(lapply(
      randomizer[["subs",1,1]][whichQuestParamsChange],
                                    unlist), function(x) do.call(paste, as.list(c("trial", x, sep=""))))
  }
  
  changing.conditions <-
    lapply(runs$beforeRun.trials.randomizers,
           function(x)whatChanges(x[[1]]))

  changing.conditions <- lapply(changing.conditions, function(x) {
    if (any(matches <- grep("ccluders", x))) {
      #A thing I do not like about R: this is the easiest way to drop elements from a vector.
      #Think about a fix for the sanity package
      x <- x[-matches]
      x <- c(x,"visibilityCondition")
    }
    #nTargets always varies... ignore it.
    if (any(matches <- grep("nTargets", x))) {
      x <- x[-matches]
    }
    x
  })

  #"conditions" determines how we organize the lattice plot, below.
  conditions <- Reduce(union, (changing.conditions))
  #not sure what the double dot is doing there but eh
  conditions <- setdiff(conditions, c("trial..extra.r", "trial.extra.r"))
}

common.manipulations <- function(env) {
  with(env, {
    ##we need some special handling for the conditions in the occluder
    ##experiments. (left/right/none)
    if (!is.null(trials$trial.occluders)) {
      trials <- transform(trials, visibilityCondition=
                          factor(mapply(function(occluder, useOccluder) {
                            if(is.na(useOccluder))
                              "full"
                            else if (useOccluder) {
                              if (occluder[[1]][["startAngle",1,1]] > pi) "left" else "right"
                            } else "full"
                          }, trials$trial.occluders, trials$trial.useOccluders),
                                 levels=c("left", "right", "full"))
                          )
    } else {
      trials$visibilityCondition <- factor("full", levels=c("left", "right", "full"))
    }
    
    ##determine for each trial if the "correct" response was given
    trials <- transform(trials,
                        correct=(result.response
                                 ==( - trial.extra.globalDirection
                                     - trial.extra.localDirection * !trial.extra.globalDirection)))
    ##mark the motion condition in each trial based on whether local and
    ##global run the same direction.
    trials <- transform(trials,
                        motionCondition=
                        factor(array(c("congruent","ambivalent","incongruent",
                                       "local",NA,"local",
                                       "incongruent","ambivalent","congruent"),c(3,3))
                               [cbind(trial.extra.localDirection+2,trial.extra.globalDirection+2)],
                               levels=c("local","ambivalent","congruent","incongruent")))

    ##compute the response time as follows.

    ##Find the 'cw' or 'ccw' trigger for each trial.
    t.responseTimestamp <- triggers[grep("cw",triggers$name),c("trials.i", "knobTime")]
    colnames(t.responseTimestamp) <- c("i", "responseTimestamp")
    trials <- merge(trials, t.responseTimestamp, all.x=TRUE)

    ##find the timestamp of motion onset (this is before the actual first
    ##element, as given by motionFirstElementDelay)
    t.motionBegun <- triggers[triggers$name == "ConcentricTrial/run/startMotion",
                              c("trials.i", "next")]
    colnames(t.motionBegun) <- c("i", "motionBegun")
    trials <- merge(trials, t.motionBegun, all.x=TRUE)

    trials <- transform(trials, responseTime = (responseTimestamp - motionBegun - trial.motion.process.t))
    if (is.null(trials$trial.maxResponseLatency)) trials$trial.maxResponseLatency <- NA

    ##Determine whether the response time was in the allowable range for
    ##each trial
    trials <- transform(trials, minResponseTime = trial.awaitInput - trial.motion.process.t)
    trials <- transform(trials, maxResponseTime = ifelse(is.na(trial.maxResponseLatency),
                                  Inf, trial.maxResponseLatency)
                        + minResponseTime)
    trials <- transform(trials, responseInWindow = (responseTime<maxResponseTime) & (responseTime > minResponseTime))

    ##pull out the subject ID into each trial.
    runs$subject <- unlist(lapply(runs$afterRun.params,
                                  function(x) ifelse(length(dim(x))==3, x[['subject',1,1]], NA)),
                           recursive=FALSE)
    trials <- merge(trials, runs[,c("i" ,"subject")],
                    by.x='runs.i', by.y='i',
                    all.x=TRUE)
  })
}

common.manipulations.variations <- function(env) {
  with(env, {
      ##oops...
    trials <- within(trials, subject[subject=="dtdt"] <- "dt")
  
    ## work out what we vere varying during these experiments...
    condition.columns <- what.varies(runs)
    
    ##here we put special case handling of things.
    condition.exprs <- parse(text=condition.columns)

    trials <-
      within(trials,
             target.spacing <- 2 * pi * trial.motion.process.radius / trial.extra.nTargets)
    trials <- subset(trials, trial.version...function == "ConcentricTrial")
    trials <- transform(trials, log.target.spacing=log(target.spacing), target.spacing=NULL)

    ##strip away data/columns we aren't planning on using, for the time being.
    trials <- subset(trials, select=c(condition.columns,
                               'trial.motion.process.radius', 'result.success',
                               'log.target.spacing', 'motionCondition', 'subject',
                               'responseInWindow', 'responseTime', 'minResponseTime',
                               'maxResponseTime', 'correct', 'runs.i', 'trial.extra.nTargets'))
    rm(triggers)
    rm(runs)
    rm(frame.skips)
  })
}
