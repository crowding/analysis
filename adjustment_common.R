#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(plyr)
  library(ptools)
})

`%--%` <- setdiff
`%++%` <- union

common <- function(infile, outfile) {
  env <- new.env(parent=.GlobalEnv)
  suppressPackageStartupMessages({
    load(infile, envir=env)
  })
  
  with(env, {

    trials <- colwise(unclass)(trials)
    triggers <- colwise(unclass)(triggers)
    stimuli <- colwise(unclass)(stimuli)
    
    useful.columns <-
      chain(colnames(trials)
            , . %--% grep("version", ., value=TRUE)
            , . %--% grep("svn", ., value=TRUE)
            , grep("^trial.extra", ., value=TRUE) %++% grep("^result", ., value=TRUE)
            , . %++% c("i", "runs.i", "trial.version__.function"
                       , "trial.parameter", "trial.parameterValues")
            , . %--% c(  "result.motion.process.angle"
                       , "result.motion.process.phase"
                       , "result.motion.process.velocity"
                       , "trial.extra.phase")
            )

    triggers <- mutate(triggers,
                       name = sub("(ConcentricAdjustmentTrial|ConcentricAdjustmentPeriodicTrial)"
                         , "", name))

    trials <-
      chain(trials 
            , subset(trial.version__.function
                     %in% c("ConcentricAdjustmentPeriodicTrial", "ConcentricAdjustmentTrial")
                     , select = useful.columns
                     )
            , mutate(.
                     , par.min = vapply(trial.parameterValues, min, 0)
                     , par.max = vapply(trial.parameterValues, max, 0)
                     )
            )

    stimuli <- stimuli[ !grepl("svn", colnames(stimuli)) ]
    
    trials$adjusting <- paste("trial.", trials$trial.parameter, sep="")
    adjusting <- paste("trial.", unique(trials$trial.parameter), sep="")

    
    parameters <-
      chain(trials
            , colwise(function(x) length(unique(x)))(.)
            , unlist
            , names(.)[.>1]
            , . %--% adjusting
            , grep('^trial\\.extra', ., value=TRUE)
            , . %--% c("trial.extra.phase", "trial.extra.globalDirection"
                       , "trial.extra.localDirection")
            )

    ## give the conditions a label...
    
    orderby <- trials[,"trial.parameter" %++% parameters %++% adjusting]
    ##construct trial labels...
    col.to.na <- match( paste("trial.", orderby$trial.parameter, sep="")
                       , colnames(orderby))
    indices <- array(FALSE,dim=dim(orderby))
    indices[cbind(1:nrow(orderby), col.to.na)] <- TRUE
    orderby[indices] <- NA
    rn <- do.call(paste, c(format(orderby, digits=3)))
    trials$label <- rn
    ##trials <- trials[do.call(order, orderby),]
                                        #for idempotence
    ##orderby <- orderby[do.call(order, orderby),]

    ##Now work out what the individual stimuli were.
    ##Note that the nice "extra" columns were not recorded, so we back them out.
    computeContrast <- mkchain(  vapply(function(x) if(dim(x)[2] > 1) x[1,1:2] else c(1,0)
                                        , c(0,0))
                               , t
                               , (.[,1] - .[,2]) / (.[,1] + .[,2]))
    
    stimuli <-
      chain(  stimuli
            , subset(select= ! (colnames(stimuli)
                                %in% c("trial.extra.globalDirection"
                                       , "trial.extra.localDirection"
                                       , "adjusting")))
            , merge(  trials[,c("i"
                                , "trial.extra.globalDirection"
                                , "trial.extra.localDirection"
                                , "adjusting")]
                    , by.x="trials.i", by.y="i")
            , mutate( trial.extra.globalVScalar =
                     (stimulus.process.dphase
                      / stimulus.process.dt
                      * sign(trial.extra.globalDirection))
                     , trial.extra.directionContrast =
                     computeContrast(stimulus.process.color) * trial.extra.localDirection
                     , trial.extra.nTargets=
                     vapply(stimulus.process.phase,
                            function(x)length(unique(x)),0))
            )
    
    ##Okay. Now we gather information about each stimulus presentation
    ##(i.e. how long it lasted, and what the adjustment value was.)
    ##When the events are logged, first there is a stimulus, then there is a
    ##startMotion, then there is acceptance, rejection, or reconfigure
    ##(all of which stop the stimulus.)
    ##
    ##First let's extract all the stimulus presentations, which entails
    ##a stimulue being logged, followed by a startMotion, followed by a
    ##number of ways to end the stimulus presentation
    ##
    ##Turns out the best way to actually _do_ the searching for these
    ##subsequences in the sequence of events is by regular expressions!
    chain(triggers
          , trimmed<-subset(.,select=c("i", "trials.i", "name", "next.", "stimuli.i"))
          , summarize(  stim  = (name == "BEGIN STIMULUS")
                      , start = (name == "/run/startMotion")
                      , stop  = (name %in% c("/run/reconfigure"
                                             , "/run/acceptIfShownAndAdjusted"
                                             , "/run/accept"
                                             , "/run/reject"
                                             ))
                      )
          , with(ifelse(stim, 'a', ifelse(start, 'b', ifelse(stop, 'c', '-'))))
          , paste(collapse='')
          , gregexpr('(?<stim>a)[^abc]*(?<start>b)[^abc]*(?<stop>c)', ., perl=TRUE)[[1]]
          , attr("capture.start")
          , as.data.frame
          , lapply(function(x)trimmed[x,])
          , do.call(`data.frame`, .)
          ) -> presentations
    
    presentations <-
      chain(stimuli[,c("trials.i", "i"
                       , "trial.extra.nTargets"
                       , "trial.extra.directionContrast"
                       , "trial.extra.globalVScalar"
                       , "stimulus.process.t"
                       , "stimulus.process.dt"
                       , "stimulus.process.n")
                    %++% adjusting]
            , merge(presentations, by.x = "i", by.y="stim.stimuli.i")
            )

    presentations <-
      chain(presentations
            , mutate(  onset = start.next. + stimulus.process.t
                     , offset = pmin(onset + stimulus.process.dt * stimulus.process.n , stop.next.)
                     , duration = pmax(offset - onset, 0))
            , subset(duration > 0)
            )

    ##Pull out the relevant information for how each trial started,
    ##ended, and each stimulus presentations, into data frames
    ##"startings", "endings," "showings".
    ##Common columns: i, value, parameter, type, time, alignedTime,
    ##duration, label, par.min, par.max, rel.par
    
    endings <-
      merge(  chain(trials
                    , mutate(  value=result.parameterValue
                             , rel.value = (result.parameterValue - par.min)
                             / (par.max - par.min) * 2 - 1
                             , type = ifelse(result.accepted, 'a', 'r')
                             )
                    , .[,c("i", "adjusting", "value", "rel.value", "par.min", "par.max"
                           , "label", "type", adjusting, parameters)])
            , chain(triggers
                    , subset(name %in% c("/run/accept"
                                         , "/run/reject")
                             , select = c('next.', 'trials.i') )
                    , rename(c(next. = "endingTime", trials.i = "i"))
                    , mutate(alignedTime = 0))
            , by = "i"
            )
    index(endings, col=endings$adjusting) <- endings$value

    ##it turns out "startings" is irrelevant because the stimulus parameter is computed de
    ##novo for the first "showing."
    
    ## startings <-
    ##   chain(  ddply(  triggers, "trials.i"
    ##                 , function(x) rename(x[order(x$i)[1],]
    ##                                      , c(next.="startTime", trials.i = "i")))
    ##         , merge(trials[, c(  "i", "label", "par.min", "adjusting"
    ##                            , "par.max"
    ##                            , adjusting, parameters)]
    ##                 , by="i")
    ##         , merge(endings[,c("endingTime", "i")], by="i")
    ##         , mutate(  .
    ##                  , value = index(., col=.$adjusting)
    ##                  , type='s'
    ##                  , alignedTime=startTime - endingTime)
    ##         )

    ##Each presentation
    showings <-
      chain(presentations
            , .[,c("start.trials.i", "onset", "duration", adjusting)]
            , mutate(type="p")
            , rename(c(onset="presentationTime", start.trials.i="i"))
            , merge(endings[,c("i", "adjusting", "endingTime", "label", "par.min", "par.max")], by="i")
            , mutate(  .
                     , value = index(., col=adjusting)
                     , rel.value = (value - par.min) / (par.max - par.min) * 2 - 1
                     , alignedTime = presentationTime - endingTime)
            )

    ##Also I want each knob turn on there.
    turns <-
      chain(triggers
            , subset(  name=="/run/knobTurned"
                     , select=c('trials.i', 'knobRotation', 'knobPosition', 'knobTime'))
            , mutate(  type = ifelse(sign(knobRotation) > 0, 'u', 'd'))
            , rename(c(trials.i="i"))
            , merge(endings[,c("i", "endingTime", "label")], by="i")
            , mutate(alignedTime = knobTime - endingTime)
            )
    
    adjusting <- mutate(expand.grid(runs.i=runs$i, adjusting=adjusting)
                         , i=seq(length(runs.i)))
    
    parameters <- mutate(expand.grid(runs.i=runs$i, parameters=parameters)
                         , i=seq(length(runs.i)))

    endings <- rename(endings, c(i="trials.i"))
    showings <- rename(showings, c(i="trials.i"))
    turns <- rename(turns, c(i="trials.i"))
  })
  
  save(envir=env,
       list=c("trials", "runs", "triggers", "stimuli"
         , "showings", "presentations", "endings", "turns"
         , "adjusting", "parameters")
       , file=outfile)
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly = TRUE)
  do.call('common', as.list(args))
}

