#!usr/bin/env Rscript
invisible(capture.output({
  source("common.manipulations.R")
  source("programming.R")
  source("modeling.manipulations.R")
  source("asymptote.fitting.R")
}))

#my.args <- as.list(strsplit('pools/GB_Timing.Rdata pools/GB_Timing.pdf', ' ')[[1]])
#do.call(session.graphs, my.args)

##{{{ Variations

what.varies <- function(trials) {
  pipe(trials,
       subset(trial.version__.function == 'ConcentricTrial'),
       sapply(function(x) if (mode(x) == "numeric" && !is.factor(x)) almost.unique(x) else unique(x)),
       sapply(length),
       .>1,
       which,
       names,
       setdiff(c('runs.i','source.file','i','runs.i','trial.awaitInput','correct','run.seq',
                 'trial.beepFeedback','trial.occluders','trial.desiredResponse',
                 'trial.earlyTimeout','trial.startTime','trial.extra.globalDirection',
                 'trial.extra.localDirection','motionBegun','trial.feedbackFailedFixation',
                 'trial.extra.nTargets', 'trial.extra.phase', 'params',
                 'trial.audioCueTimes', 'trial.useOccluders', 'trial.eyeData',
                 'params.i', 'trial.i',
                 'trial.extra.min.extent','trial.extra.max.extent')),
       .[grep('^(response|result|trial.fixation|trial.motion|trial.textFeedback)',., invert=TRUE)],
       .[grep('(\\.svn|\\.\\.\\.)',.,invert=TRUE)]
       )
}

why.varies <- function(trials, key) {
  pipe(trials,
       .[,c(key, 'source.file')],
       colwise(function(x)if(is.numeric(x)) cluster.to.unique(x) else x)(.),
       unique,
       within(source.file <- substr(source.file, 1, 15))
       )
##       ddply(key, with,list(source.file=list(source.file))))
}

##}}}

prefix.names <- function(x, prefix) {names(x) <- paste(prefix, names(x), sep=''); x}


##{{{ all session graphs

session.graphs <- function(...) {

  library(plyr)
  library(ggplot2)
  library(gtools)
  my.args <- list(...)

  pdf(my.args[[2]], onefile=TRUE)
  keep=FALSE
  on.exit({
    dev.off()
    if (!keep)
      file.remove(my.args[[2]])
    }, add=TRUE)

  if (grepl("Titrat", my.args[[1]]) | grepl("Crit", my.args[[1]])) {
    x <- titration.plots(...)
    keep <- TRUE
    return(x)
  }

  if (grepl("Discriminability", my.args[[1]])) {
    x <- discriminability.plots(...)
    keep <- TRUE
    return(x)
  }
  
  load(my.args[[1]])
  
  ##do the basic assignments. I am lazy and will just pass the whole environment down to work on:
  common.manipulations(environment())
  trials <- subset(trials, select=colnames(trials)[lapply(trials,mode)!="list"])

  ##for the various quest experiments, work out what was changing. Possibly nothing is changing.
  conditions <- what.varies(trials)

  ##run a check on the conditions taht are varied in this pool
  problematic.conditions <- conditions[grep('^(trial.extra|motionCondition|subject|visibility|extent)',conditions,invert=TRUE)]

  if (length(problematic.conditions > 0)) {
    cat('DO YOU MEAN TO POOL DATA WHERE THESE VARY: (', paste(problematic.conditions, collapse=', '), ')\n')
    for (cond in problematic.conditions) {
      pipe(trials,
           subset(trial.version__.function == 'ConcentricTrial'),
           why.varies(cond),
           print)
    }
    conditions <- setdiff(conditions, problematic.conditions)
  }

  #even though we complain about differences in response time, we'll ignore them.
  conditions <- setdiff(conditions, c('minResponseTime', 'maxResponseTime'))

  trials <- subset(trials,
                   trial.version__.function == 'ConcentricTrial'
                   & !is.na(correct))
  trials <- mutate(trials,
                   target.spacing = 2*pi*trial.extra.r/trial.extra.nTargets,
                   log.target.spacing = log(target.spacing))
  if (length(conditions) > 3) {
    conditions <- setdiff(conditions, "motionCondition")
    trials <- subset(trials, motionCondition == "incongruent")
  }
  if (length(conditions) > 3) {
    for (cond in conditions) {
      pipe(trials,
           subset(trial.version__.function == 'ConcentricTrial'),
           why.varies(cond),
           print)
    }
    stop("Too many conditions: ", paste(conditions, collapse=", "))
  }
  
  pCorrect <- ddply(subset(trials, responseInWindow == TRUE),
                    c('trial.extra.nTargets', conditions),
                    summarize,
                    pCorrect=mean(correct),
                    n = length(correct),
                    log.target.spacing=unique(log.target.spacing))
  nlimits <- with(pCorrect, c(min(n),max(n)))
  
    ##we have one, two or three conditions. Or maybe more?
    plot.expr <- quote(
      (ggplot(pCorrect)
       + aes(x=log.target.spacing, y=pCorrect)
       + geom_point(aes(size=n))
       + scale_area(limits=nlimits, to=sqrt(5*nlimits/max(nlimits)), breaks=c(1, 2, 5, 10, 20, 50))
       + theme_bw() + opts(panel.grid.major = theme_blank(),panel.grid.minor = theme_blank())
       + foo))

    plot.expr <-
      substitute.nq(plot.expr,
                    list(foo=switch(length(conditions),
                           quote(facet_grid(condition1 ~ .)),
                           quote(facet_grid(condition1 ~ .)),
                           quote(facet_grid(condition1 ~ condition3))
                           )))
    if (length(conditions) >= 2) {
      plot.expr <- substitute(. + aes(color=factor(condition2), fill=factor(condition2)), list(.=plot.expr))
    }

    ##how about a sequential plot?
    seq.plot.expr <- quote(ggplot(trials)
     + aes(x=i, y=log.target.spacing, shape=correct, alpha=ifelse(responseInWindow, 1, 0.3))
     + geom_point()
     + scale_shape_manual(values=c(1,20))
     + scale_alpha(name="response latency", breaks=c(0.3, 1), to=c(0.3, 1), labels=c("outside window", "in window"))
     + theme_bw() + opts(panel.grid.major = theme_blank(),panel.grid.minor = theme_blank())
     + scale_x_continuous(name="trial number")
     )

    seq.plot.expr <- substitute.nq(switch(length(conditions),
        quote(seq.plot.expr + facet_grid(condition1 ~ .)),
        quote(seq.plot.expr + facet_grid(condition1 ~ condition2)),
        quote(seq.plot.expr + facet_grid(condition1 ~ condition3) + aes(color=factor(condition2)))),
                                   list(seq.plot.expr = seq.plot.expr))

    ## and a response time plot?
    response.plot.expr <- quote(ggplot(trials)
       + aes(x = trial.extra.nTargets, y = responseTime,
             color=correct, alpha=ifelse(responseInWindow, 1, 0.3))
       + scale_alpha(name="response latency", breaks=c(0.3, 1), to=c(0.3, 1), labels=c("outside window", "in window"))
       + theme_bw() + opts(panel.grid.major = theme_blank(), panel_grid_minor = theme_blank())
       + geom_point(position=position_jitter(w = 0.5, h = 0), size=0.7)
       + coord_cartesian(ylim = c(0,1))
       )

    response.plot.expr <- substitute.nq(switch(length(conditions),
        quote(response.plot.expr + facet_grid(condition1 ~ .)),
        quote(response.plot.expr + facet_grid(condition1 ~ condition2)),
        quote(response.plot.expr + facet_grid(condition1 ~ condition3) + aes(shape=factor(condition2)))),
                                   list(response.plot.expr = response.plot.expr))
    
    ##and a plot of direction bias?
    ##and some metric of response bias and serial dependence?
              
    ## Try fitting basic psych functs.
    got.points <- FALSE
    tryCatch({
      
      ## 1. One for incongruent and the other for counterphase.
      regression.formula <-
        pipe(conditions,
             setdiff("motionCondition"),
             paste("factor(", ., ")", sep=""),
             c("log.target.spacing",.),
             paste(collapse="*"),
             paste("correct ~", .),
             parse(text=.),
             eval
             )
      library(psyphy)
      model <- glm(regression.formula, family=binomial(logit.2asym(0.10, 0.01)),
                   subset(trials, responseInWindow & motionCondition=='incongruent'))

      ## prediction points
      plotpoints <- ddply(trials, conditions, with,
                          data.frame(log.target.spacing=
                                     seq(min(log.target.spacing)-0.5,
                                         max(log.target.spacing)+0.5, len=100)))
      plotpoints <- cbind(plotpoints, predict(model, plotpoints, type="link", se.fit=TRUE))
      plotpoints <- mutate(plotpoints,
                           pCorrect=model$family$linkinv(fit),
                           pCorrect.lower=model$family$linkinv(fit-se.fit),
                           pCorrect.upper=model$family$linkinv(fit+se.fit))
      if ("motionCondition" %in% colnames(plotpoints)) {
        plotpoints <- subset(plotpoints, motionCondition=="incongruent")
      }
      plot.expr <- substitute(foo + geom_line(data=plotpoints)
                              + geom_ribbon(data=plotpoints,
                                            aes(ymin=pCorrect.lower,
                                                ymax=pCorrect.upper),
                                            alpha=0.2, linetype=0),
                              list(foo = plot.expr))

      ## intercepts and error bars
      require(arm)
      model$sim <- sim(model, 500)
      cases <- ddply(trials, conditions, nrow)
      center.est <- find.intercept(model,cases, 'log.target.spacing')
      condition.stats <- sim.statistic(model, find.intercept, cases, 'log.target.spacing', .progress="text")
      
      condition.stats <- ddply(condition.stats, conditions, with,
            prefix.names(summary(log.target.spacing), 'log.target.spacing.'))
      condition.stats <- merge(condition.stats, center.est, by=conditions)
      got.points <- TRUE
      
    }, error = function(ex) {
      print("could not fit model?")
      print(ex)
    })
             
    cond.names <- sapply(conditions, as.name)

    perms <- pipe(conditions, length, if(. > 0) permutations(.,.) else array(numeric(0), c(1,0)),
                  .[switch(length(conditions), c(1), c(1,2), c(1,3,5)),,drop=FALSE])
    aaply(perms, 1,
          function(perm) {
            names(cond.names) <- paste('condition', 1:length(cond.names), sep='')[perm]
            the.plot.expr <- substitute.nq(plot.expr, cond.names)
            seq.plot.expr <- substitute.nq(seq.plot.expr, cond.names)
            response.plot.expr <- substitute.nq(response.plot.expr, cond.names)
            print(eval(the.plot.expr))
            if (length(unique(trials$source.file)) <= 1)
              print(eval(seq.plot.expr))
            print(eval(response.plot.expr))
            0
          })

    if (got.points && 'trial.extra.r' %in% conditions) {
      rest.conds <- setdiff(conditions, c('trial.extra.r', 'motionCondition'))
      
      intercept.plot.expr <- quote(ggplot(condition.stats)
                                   + aes(x=log(trial.extra.r),
                                         y=log.target.spacing,
                                         ymin=`log.target.spacing.1st Qu.`,
                                         ymax=`log.target.spacing.3rd Qu.`)
                                   + geom_point() + geom_line()
                                   + geom_errorbar(width=0.1)
                                   + theme_bw()
                                   + opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), aspect.ratio=1)
                                   + coord_cartesian(ylim=c(-0.5, 2), xlim=c(0.5,2.5))
                                   )
      intercept.plot.expr <-
        substitute.nq(switch(length(rest.conds) + 1,
                             quote(foo),
                             quote(foo + aes(color=factor(condition1))),
                             quote(foo + facet_grid(condition2 ~ .) + aes(color=factor(condition2))),
                             quote(foo + facet_grid(condition2 ~ condition3) + aes(color=factor(condition1)))),
                      list(foo=intercept.plot.expr))

      rest.perms <- pipe(rest.conds, length, if(. > 0) permutations(.,.) else array(numeric(0), c(1,0)))
      cond.names <- sapply(rest.conds, as.name)
      adply(rest.perms, 1, function(perm) {
        names(cond.names) <- paste('condition', 1:length(cond.names), sep='')[perm]
            the.plot.expr <- substitute.nq(intercept.plot.expr, cond.names)
            print(eval(the.plot.expr))
          })
    }
  keep <- TRUE
  
}

##}}}

##{{{ Discriminability experiment

#args <- as.list(strsplit("session_graphs.R unpacked/pbm-2011-09-16__20-02-10-ConcentricDirectionDiscriminability.RData session_figures/pbm-2011-09-16__20-02-10-ConcentricDirectionDiscriminability.pdf", " ")[[1]])[-1]
#do.call(discriminability.plots, args)
discriminability.plots <- function(...) {

  invisible(capture.output({
    require(plyr)
    require(ggplot2)
    require(gtools)
    require(psyphy)
  }))
  
  theme_set(theme_bw())
  theme_update(panel.grid.major = theme_blank(),
               panel.grid.minor = theme_blank())
  
  my.args <- list(...)
  load(my.args[[1]])
  common.manipulations(environment())

  #rev(sort(sapply(what.varies(trials), function(x) length(unique(trials[,x])))))

  #now the thing is, my trials ar all either local-only or global-only?
  trials <- mutate(trials,
                   displacement=trial.extra.r
                   * trial.motion.process.dt
                   * trial.extra.globalVScalar,
                   interest = ifelse(
                     motionCondition == "local",
                     trial.extra.directionContrast,
                     displacement),
                   spacing=pipe(target.spacing, ordered, factor(.,
                     labels=format(as.numeric(levels(.)),
                       digits=3,nsmall=2))),
                   rawrow=interaction(subject,spacing))
  
  rates <- pipe(trials
      , subset(responseTime >= 0.4 & responseTime <= 0.9)
      , ddply(c("spacing","target.spacing","log.target.spacing","subject","rawrow",
                "trial.extra.directionContrast", "motionCondition",
                "displacement"),
              summarize,
              n = length(correct), p = mean(correct),
              success = sum(correct), failure = sum(!correct)
              )
      , mutate(interest = ifelse(motionCondition == "local",
                    trial.extra.directionContrast, displacement))
      )  

  #make a raw data plot
  lablr <- function(variable, value){
                  if (variable=="motionCondition") {
                    c(local="Direction contrast", counterphase="Displacement (deg.)")[value]
                  } else value
                }
  
  print(dot.plot <- ggplot(rates)
   + aes(x=interest, y=p, size=n, color=spacing, group=1)
   + geom_point()
   + scale_area("observations")
   + facet_grid(subject ~ motionCondition, scale="free_x",
                labeller=lablr)
   + scale_x_continuous(name="")
   + scale_y_continuous(name="Prop. correct")
   + opts(aspect.ratio=1)
   )

  #lessa do it with reaction times.
  print(time.plot <- ggplot(trials)
   + aes(interest, responseTime, color=ordered(correct),group=1)
   + facet_grid(rawrow ~ motionCondition, scale="free_x", labeller=lablr)
   + geom_point()
   + scale_color_hue("correct")
   + scale_x_continuous(name="")
   )

  thresh <- 2^(-1/3)
  #and stacks...
  print(stack.plot <- ggplot(rates)
   + aes(interest, success)
   + facet_grid(rawrow ~ motionCondition, scale="free_x", labeller=lablr)
   + geom_line(color="blue")
   + geom_line(aes(y = failure*2^(-1/3)/(1-2^(-1/3))), color="red")
   + scale_x_continuous(name="")
   )

  #and why not sequentially.
  trials <- ddply(trials, .(rawrow), mutate, o=order(i))
  
  print(sequential.plot <- ggplot(trials)
        + aes(o, interest, color=spacing, shape=correct)
        + geom_point()
        + facet_grid(subject ~ motionCondition, scale="free_x", labeller=lablr)
        + scale_shape_manual(breaks=c(F,T), values=c(1,20)) 
        )       
  
  #fit logit functions. Since staircase, no worry abt asymptotes.
  if (with(rates, length(unique(subject)) > 1)) {
    fmla <- cbind(success,failure) ~ interest*spacing*subject    
  } else {
    fmla <- cbind(success,failure) ~ interest*spacing
  }
  
  if (with(rates, length(unique(motionCondition)) > 1)) {
    fmla <- update.formula(fmla, . ~ .*motionCondition)
  }
  
  model <- glm(data=rates, fmla,
           family=binomial(link=logit.2asym(0.5, 0)))
  
  tryCatch(error=function(e)NaN, {
    rates[,c('predict','se.fit')] <- predict(model, type="response", se.fit=TRUE)[1:2]

    print(ggplot(rates)
          + aes(x=interest, y=predict, ymin=predict - se.fit, ymax=predict+se.fit, group=spacing, fill=spacing, color=spacing)
          + geom_line()
          + geom_ribbon(alpha=0.25, color=NA)
          + geom_point(aes(y = p, size=n), alpha=0.5)
          + scale_area()
          + facet_grid(subject ~ motionCondition, scale="free_x",
                       labeller=lablr)
          + scale_x_continuous(name="")
          + scale_y_continuous(name="Prop. correct")
          + opts(aspect.ratio=1)
          )
  })
  
}

##}}}

##{{{ Titration experiments

titration.plots <- function(...) {

  invisible(capture.output({
    require(plyr)
    require(ggplot2)
    require(gtools)
    require(psyphy)
  }))

  theme_set(theme_bw())
  theme_update(panel.grid.major = theme_blank(),
               panel.grid.minor = theme_blank())
  
  my.args <- list(...)

  load(my.args[[1]])
  common.manipulations(environment())
  
  conditions <- setdiff(what.varies(trials), "log.target.spacing")
  pipe(conditions,
       setdiff(c("trial.extra.globalVScalar", "trial.extra.directionContrast")),
       union(c("abs.localDirectionContrast", "abs.displacement"))
       ) -> abs.conditions

  ## now, maybe first this data would actually make more sense in
  ## terms of an absolute "left" vs. "right".
  mutate(trials,
         abs.localDirectionContrast =
           sign(trial.extra.localDirection) * trial.extra.directionContrast,
         abs.displacement =
           sign(trial.extra.globalDirection) * trial.extra.globalVScalar
           * trial.extra.r * trial.motion.process.dt,
         abs.response = -result.response,
         spacing=pipe(target.spacing, ordered, factor(.,
                     labels=format(as.numeric(levels(.)),
                       digits=3,nsmall=2))),
         ) -> trials

  #direction of 1 is counterclockwise I believe.
  mutate(
         ddply(subset(trials, responseTime >= 0.4 & responseTime <= 0.9),
               union(abs.conditions, c("subject", "spacing")),
               summarize,
               n.cw = sum(sign(abs.response) == -1),
               n.ccw = sum(sign(abs.response) == 1),
               n = sum(sign(abs.response) != 0)
               ),
         p.cw = n.cw / n,
         p.ccw = n.ccw / n
         ) -> abs.rates

  #fit a bunch of curves
  got.fits <- FALSE
  abs.fitted <- abs.rates
  try({
    ddply(abs.rates, setdiff(abs.conditions, "abs.displacement"), function(chunk) {
      model <- glm(cbind(n.ccw, n.cw) ~ abs.displacement,
                   data=chunk,
                   family=binomial(link="logit"))
      pred <- predict(model, type="response", se.fit=TRUE)
      with(pred, data.frame(chunk, fit, se.fit))
    }) -> abs.fitted
    got.fits <- TRUE
  })

  ##plot and fit a bunch of curves and points
  (ggplot(abs.fitted)
   + aes(abs.displacement, p.ccw, color=factor(abs.localDirectionContrast),
         fill = factor(abs.localDirectionContrast))
   + geom_point(aes(size=n), alpha=0.5)
   + scale_area("observations")
   + facet_grid(subject ~ spacing)
   + opts(aspect.ratio=1)
   + scale_color_hue("direction contrast")
   + scale_fill_hue("direction contrast")
   ) -> abs.plot

  if (got.fits) {
    abs.plot <- (abs.plot + geom_line(aes(y=fit))
                 + geom_ribbon(alpha=0.3, linetype=0,
                               aes(ymin = fit - se.fit, ymax = fit + se.fit)))
  }

  print(abs.plot)

  ##Also, plot this with the facets being of direction contrast.
  print(abs.plot
        + facet_grid(subject ~ abs.localDirectionContrast)
        + aes(color=spacing, fill=spacing)
        + scale_color_hue("spacing")
        + scale_fill_hue("spacing")
        )
       
  ##Huh, it looks like a logistic function does not capture the
  ##behavior for crowded stimuli at zero motion contrast. What gives?
  ##Spatial aliasing???

  ##after we've looked at absolutes, let's fold the clockwise and
  ##counterclockwise directions over each other and see if that makes
  ##sense.  Note that in case of zero direction contrast I am folding
  ##the displacement over itself so we only get half a curve there.
  ##Now, what I want to do is extract estimates of SLOPE and BIAS from these data.
  ##fitting a model,
  mutate(trials,
         folded.localDirectionContrast = abs(abs.localDirectionContrast),
         folded.displacement =
            ifelse(abs.localDirectionContrast != 0,
                   abs.displacement * sign(abs.localDirectionContrast),
                   abs.displacement * sign(abs.displacement)),
         folded.response =
           ifelse(abs.localDirectionContrast != 0,
                  abs.response * sign(abs.localDirectionContrast),
                  abs.response * sign(abs.displacement))
         ) -> trials

  pipe(abs.conditions,
       setdiff(c("abs.displacement", "abs.localDirectionContrast")),
       union(c("folded.localDirectionContrast", "folded.displacement"))
       ) -> folded.conditions

  #and we finally have to discard the "perfectly ambiguous" stimuli...
  mutate(  ddply(  subset(trials
                          , responseTime >= 0.4
                          & responseTime <= 0.9
                          & folded.response != 0)
                 , union(folded.conditions, c("subject", "spacing"))
                 , summarize
                 , yes = sum(folded.response > 0)
                 , no = sum(folded.response < 0)
                 )
         , p = yes/(yes+no)
         , n = yes + no
         ) -> folded.rates

  got.folded.fits <- FALSE
  folded.fitted <- folded.rates
  try({
    ddply(folded.rates, setdiff(folded.conditions, "folded.displacement"), function(chunk) {
      model <- glm(cbind(yes, no) ~ folded.displacement,
                   data=chunk,
                   family=binomial(link="logit"))
      pred <- predict(model, type="response", se.fit=TRUE)
      with(pred, data.frame(chunk, fit, se.fit))
    }) -> folded.fitted
    got.folded.fits <- TRUE
  })

  ##plot and fit a bunch of curves and points
  (ggplot(folded.fitted)
   + aes(folded.displacement, p,
         color=factor(folded.localDirectionContrast),
         fill = factor(folded.localDirectionContrast))
   + scale_area()
   + geom_point(aes(size=n))
   + facet_grid(subject ~ spacing)
   + opts(aspect.ratio=1)
   + scale_color_hue("direction contrast")
   + scale_fill_hue("direction contrast")
   + geom_vline(x = 0, alpha=0.3)
   + geom_hline(y = 0.5, alpha=0.3)
   ) -> folded.graph
  ##also let's try with 

  if (got.folded.fits) {
    folded.graph <- (folded.graph
                     + geom_ribbon(alpha=0.3, linetype=0,
                                   aes(ymin = fit - se.fit, ymax = fit + se.fit))
                     + geom_line(aes(y=fit))
                     )
  }
  print(folded.graph)

  print(folded.graph
        + facet_grid(subject ~ folded.localDirectionContrast)
        + aes(color=spacing, fill=spacing)
        + scale_color_hue("spacing")
        + scale_fill_hue("spacing")
        )

  ##but really, WHAT THE FUCK is up with that direction contrast???
  ##ugh, it is a motion energy effect.
  ##which means I need to calculate motion energy...
}

##}}}

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call("session.graphs", as.list(my.args))
}
