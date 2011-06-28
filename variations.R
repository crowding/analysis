## Now here I start to look at the control experiments where I had variations.

##common location for modeling functions
prodfile <- ""
fit.model <- function() 0; ## to put one in the namespace??
source("common.manipulations.r")
source("modeling.manipulations.R")
source("programming.R")
source("asymptote.fitting.R")

##we run one of these for each "variation pool"
my.args <- c("pools/SF.Rdata", "pools/SF.products")
psych.fits <- function(...) {
  my.args <- list(...)

  require(plyr)
  require(ggplot2)
  require(boot)
  require(utils)
  require(arm)

  load(my.args[[1]])

  common.manipulations(environment())
  common.manipulations.variations(environment())

  prodfile <<- my.args[[2]]

  ##Calculate proportion correct among trials where the subject
  ##answered within the permitted time bounds.
  pCorrect <-
    ddply(subset(trials, responseInWindow & !is.na(responseTime)),
          c('subject', 'trial.motion.process.radius', 'motionCondition',
            'log.target.spacing', condition.columns),
          function(x) c(n = nrow(x), pCorrect=mean(x$correct)))

  #open output files: first a file to list the output files
  clearfile(prodfile)

  ##{{{ plotting the raw data.

  ##Plot this raw data for the incongruent-motion trials.
  raw.data.plot <- 
    eval(bquote(ggplot(subset(pCorrect, motionCondition=='incongruent'),
                       aes(log.target.spacing,
                           pCorrect,
                           colour = factor(.(condition.exprs[[1]]))),
                       ) +
                geom_point() +
                opts(aspect.ratio=0.5) + 
                facet_grid(trial.motion.process.radius ~ subject) +
                opts(title=sprintf("raw QUEST data, varying %s",
                       paste(condition.columns, collapse=", ")))))
  
  print(raw.data.plot)
  print.to.pdf(raw.data.plot)
  
  raw.data.plot.sized <- 
    eval(bquote(ggplot(subset(pCorrect, motionCondition=='incongruent'),
                       aes(log.target.spacing,
                           pCorrect,
                           colour = factor(.(condition.exprs[[1]]))),
                       )
                +geom_point(aes(size=n)) + scale_area()
                +opts(aspect.ratio=0.5)
                +facet_grid(trial.motion.process.radius ~ subject)
                +opts(title=sprintf("raw QUEST data, varying %s",
                        paste(condition.columns, collapse=", ")))))
  print.and.pdf(raw.data.plot.sized)

  ##how about a raw data plot with ambiguous and other
  ##conditions. Collapse over number of targets
  pCorrect.collapse <-
    ddply(subset(trials, responseInWindow & !is.na(responseTime)),
          c('subject', 'trial.motion.process.radius', 'motionCondition',
            'trial.extra.nTargets', condition.columns),
          function(x) c(n = nrow(x), pCorrect=mean(x$correct)))


  ## GB couldn't see congruent motion at smaller wavelengths,
  ## especially when dense. So he's just not seeing the local
  ## motion. This means his data is quite suspect.  On the other hand,
  ## DT and PBM have quite a reasonable pattern of responses.  I
  ## wonder if for this quest-like data it'd be useful to plot
  ## alpha-blended density things... sort of a heat map?
  (raw.data.plot.motion <- ggplot(pCorrect.collapse)
        + aes(trial.extra.nTargets, pCorrect, size=n)
        + eval(bquote(aes(color=factor(.(condition.exprs[[1]])))))
        + facet_grid(subject ~ motionCondition)
        + geom_point()
        + opts(title="Raw data plot, collapsing across eccentricity")
        + theme_bw()
        + opts(aspect_ratio=0.5, legend.position="bottom")
        )
   print.to.pdf(raw.data.plot.motion)

  ##}}}

  ##Oops! need to make sure we keep the data from the same conditions
  ##we tested! We had an extra session with DT with a larger set. We'll not use those trials.
  trials$subject <- factor(trials$subject)

  if (condition.columns[[1]] == "trial.extra.wavelengthScalar") {
    considered.trials <- subset(trials,
                                (motionCondition == 'incongruent')
                                & responseInWindow
                                & !is.na(responseInWindow)
                                & !(eval(trial.extra.wavelengthScalar) > 0.12))
  } else {
  considered.trials <- subset(trials,
                                (motionCondition == 'incongruent')
                                & responseInWindow
                                & !is.na(responseInWindow))
  }
  ##{{{ folding the stuff into things

  ##which conditions do we care about plotting, after fitting?
  conditions <-
    ddply(considered.trials,
          c('subject', 'trial.motion.process.radius', condition.columns),
          function(df) df[1,condition.columns,drop=FALSE])

  ##allow asymptote to vary per subject
  asymptote.cases <- ddply(considered.trials, c("subject"),
                           function(x) data.frame(g=0.05, lam=0.05))
  
  fit.and.simulate.model <- function(fmla, data=considered.trials, cases=asymptote.cases, hessian=FALSE) {
    model <-
      glm.2asym(              
                fmla,
                data=data,
                link.2asym=joined.asymptote(cases),
                trace=5)

    ##Now compute Hessians and simulated draws.
    if(hessian) {
      model$hessian <- hessian(model)
      model$sim <- sim(model, 1000)
    }
    model
  }

  ##first look at a varying slope fit.
  varying.slope.fmla <- eval(bquote(correct ~ log.target.spacing
                                    * factor(trial.motion.process.radius)
                                    * factor(.(condition.exprs[[1]]))
                                    * subject))

  constant.slope.fmla <- eval(bquote(correct
                                     ~ subject*
                                     (log.target.spacing
                                      + factor(trial.motion.process.radius)
                                      * factor(.(condition.exprs[[1]]))
                                     )))

  scaling.fmla <- eval(bquote(correct ~ subject*(log.target.spacing
                                                 + log(trial.motion.process.radius)
                                                 * factor(.(condition.exprs[[1]])))))

  #fit all the "fmla"s. Note that I have disabled the Hessian, until I have better data.
  for (i in grep(".*\\.fmla$", ls(), value=TRUE)) {
    assign(sub("fmla", "model", i), fit.and.simulate.model(get(i), hessian=FALSE))
  }
  fmla.names <- grep(".*\\.fmla$", ls(), value=TRUE)
  model.names <- sub("fmla", "model", fmla.names)

  if (FALSE) {
    for (i in model.names) {
      eval(substitute(i$boots <-
                      bootstrap(i, n=100, data=considered.trials,
                                cases=c("trial.motion.process.radius", "subject",
                                  condition.columns)),
                      list(i = as.name(i))))
    }
  }

  ##oops! this is a fix...
  if (FALSE) {
    for (i in model.names)
      eval(substitute(local({
        tmp <- model
        while (! "formula" %in% class(tmp$formula))
          tmp$formula <- tmp$formula$formula
        model <<- tmp
      }), list(model=as.name(i))))
  }
  
  #the above was crap for small datasets, so let's also fit them more simply
  require(psyphy)
  fit.model <<- function(data, subset=NULL, formula=fmla) {
    ##Workaround for glm's nonstandard evaluation discussed here:
    ##https://stat.ethz.ch/pipermail/r-help/2008-February/154201.html
    eval(bquote((glm(formula, family=fam, data=data, subset=.(subset)))))
  }
  fam <- binomial(logit.2asym(.1, .025))
  model.names.complex <- paste(model.names, ".complex")
  for (i in model.names) {
    eval(substitute(cplx <- model,
                    list(cplx=as.name(paste(i,".complex",sep="")),
                         model=as.name(i))))
  }
  for (i in fmla.names) {
    assign(sub("fmla", "model", i), fit.and.simulate(get(i), data=considered.trials, iter=1000, cond=conditions))
  }
  for (i in model.names)
    eval(substitute({
      mdl$model$sim <- mdl$sim
      mdl <- mdl$model
    }, list(mdl=as.name(i))))

  #plot the max.likelihood psychometric functions from each model.
  plot.psychfuns <- function(model, data=considered.trials, plot.sims=TRUE, which.sims=1:100, coefs.name="sim") {
    plot.points <- ddply(data,
                         c("subject", "trial.motion.process.radius", condition.columns),
                         with,
                         data.frame(log.target.spacing =
                                    seq(min(log.target.spacing) - 1.5,
                                        max(log.target.spacing) + 1.5, len=100)))
    plot.points[,condition.columns] <- factor(plot.points[,condition.columns])
    plot.points$correct <- predict(model, plot.points, type="response")

    the.plot <- (ggplot(plot.points)
                 + aes(log.target.spacing, correct)
     + eval(bquote(aes(color=factor(.(condition.exprs[[1]])))))
     + geom_line()
     + facet_grid(subject ~ trial.motion.process.radius))

    if (coefs.name %in% names(model) && plot.sims) {
      ##yay! we get to run through simulations also. Since they are
      ##proportions, we arcsin-transform the results and compute
      ##the standard deviation.
      ensemble.points <- predict.ensemble(model, plot.points, type="response",
                                          which=which.sims, .progress="text", coefs.name=coefs.name)

      sd.points <-
        ddply(ensemble.points, c("subject", "trial.motion.process.radius",
                                 condition.columns, "log.target.spacing"),
              with,
              within(data.frame(correct.sd = sd(sindecompress(correct)),
                                correct = sincompress(mean(sindecompress(correct)))),
                     {correct.upper <- sincompress(sindecompress(correct + correct.sd))
                      correct.lower <- sincompress(sindecompress(correct - correct.sd))}),
              .progress="text")
      ##this will tend to underplot the SD for data very close to 0 or 1, but...
      the.plot <- the.plot + eval(bquote(geom_ribbon(aes(ymin=correct.lower,
                                                         ymax=correct.upper,
                                                         fill=factor(.(condition.exprs[[1]]))),
                                                     alpha=0.2, linetype=0, data=sd.points)))
    }
    the.plot
  }
    
  print.and.pdf(plot.psychfuns(varying.slope.model))
  print.and.pdf(plot.psychfuns(constant.slope.model))
  print.and.pdf(plot.psychfuns(scaling.model))

  if (FALSE){
    ##what the hell is going on with GB. Let's fit totally independent psychometric functions.
    gb.trials <- subset(trials, subject=="gb")
    gb.trials$trial.motion.process.radius <- factor(gb.trials$trial.motion.process.radius)
    gb.trials[,condition.columns] <- factor(gb.trials[,condition.columns])
    asymptote.cases.gb <- ddply(considered.trials,
                                c("trial.motion.process.radius", condition.columns),
                                function(x) data.frame(g=0.05, lam=0.05))
    total.fmla.gb <- eval(bquote(correct
                                 ~ log.target.spacing
                                 * trial.motion.process.radius
                                 * .(condition.exprs[[1]])))
    total.model.gb <- fit.and.simulate.model(total.fmla.gb,
                                             data=gb.trials,
                                             cases=asymptote.cases.gb)
    plot.psychfuns(total.model.gb, data=gb.trials)
    ##that is kind of ugly and doesn't really help me, though. I think
    ##GB just doesn't respond to local motion and is thus crap.
  }
    
  ##I may have to drop GB's data from here! He's at chance for global motion direction ?!

  for (m in model.names) {
      assign(sub(".model", ".intercepts", m), within(list(), {
        intercepts <-
          find.intercept(get(m), conditions,
                         "log.target.spacing",
                         response=0.50, response.name="correct")
        ##Now let's quantify uncertainty in the intercepts, using simulation runs.
        sim.intercepts <-
          sim.statistic(get(m), find.intercept,
                        conditions, "log.target.spacing",
                        response=0.75, response.name="correct",
                        .progress="text")
        ## and collapse the statistic of the intercept.
        sim.intercepts.summary <-
          ddply(sim.intercepts, colnames(conditions), summarize,
                log.target.spacing.mean = mean(log.target.spacing),
                log.target.spacing.sd = sd(log.target.spacing),
                log.target.spacing.25 = quantile(log.target.spacing, 0.25),
                log.target.spacing.75 = quantile(log.target.spacing, 0.75))
        sim.intercepts.summary <- merge(sim.intercepts.summary, intercepts)
      }
    ))
  }

  ##now make our favorite figure....
  (model.plot <- ggplot(constant.slope.intercepts$sim.intercepts.summary)
   + eval(bquote(aes(log(trial.motion.process.radius), log.target.spacing,
                     color=factor(.(condition.exprs[[1]])))))
   + geom_point()
   ## + geom_errorbar(aes(ymin=log.target.spacing.25,
   ##                     ymax=log.target.spacing.75),
   ##                width=0.2)
   + geom_errorbar(aes(ymin=log.target.spacing-log.target.spacing.sd,
                       ymax=log.target.spacing+log.target.spacing.sd),
                  width=0.2)
   + geom_line(data=scaling.intercepts$sim.intercepts.summary)
   + eval(bquote(geom_ribbon(data=scaling.intercepts$sim.intercepts.summary,
                             aes(ymin=log.target.spacing-log.target.spacing.sd,
                                 ymax=log.target.spacing+log.target.spacing.sd,
                                 fill=factor(.(condition.exprs[[1]]))),
                             linetype=0, alpha=0.3)))
   + facet_grid(~ subject)
   + theme_bw()
   + opts(panel.grid.major=theme_blank(),
          panel.grid.minor=theme_blank(),
          legend.position="bottom")
   + coord_cartesian(ylim=with(constant.slope.intercepts$sim.intercepts.summary,
                       c(min(log.target.spacing)-0.25,max(log.target.spacing)+0.25)))
   )
  print.to.pdf(model.plot)
  

  ## the insight I would like to share that I just had: the slight
  ## shift in threshold is due to changes in the strength of the local
  ## motion stimulus COMBINED with a measure of the stregth of the
  ## local motion stimulus (which we might obtain from titration or by
  ## Thus for GB, the local motion is VERY WEAK and adjusting it
  ## changes a lot! Thus, GB helped me give a hint towards looking at
  ## the motion energy, which may turn out useful in the TITRATION
  ## experiment...


  ##a more effective plot may be to plot SIZE versus CRITICAL DISTANCE
  ##for the four eccentricities.
   (size.plot <- ggplot(constant.slope.intercepts$sim.intercepts.summary)
    + eval(bquote(aes(log(.(condition.exprs[[1]])*trial.motion.process.radius), log.target.spacing,
                      color=factor(trial.motion.process.radius),
                      ymin=log.target.spacing - log.target.spacing.sd,
                      ymax=log.target.spacing + log.target.spacing.sd)))
    + geom_point()
    + geom_errorbar()
    + geom_line()
    + facet_grid(~ subject)
    + theme_bw()
    + opts(panel.grid.major=theme_blank(),
           panel.grid.minor=theme_blank(),
           legend.position="bottom")
    )
 print.to.pdf(size.plot)

  ##}}}
  
  ##print(quantilePlot + opts(title="Raw response quantiles"))
  ## that's ugly but promising. In the naive subjects I see a trend
  ## for the larger stimuli corresponding to a larger critical
  ## spacing. There's some indication that the size of the transition
  ## changes with the stimulus size, esp. in subject DT, but it's inconsistent and could be
  ## colored by the QUEST procedure.  I'd better pool this data to
  ## have a better chance of discussing the size of the transition
  ## region. So what factors do I want?

  ##What I'd like to suggest here is that there are two explanations
  ##for a shift of PSE with feature size: one is that the larger
  ##stimuli have a larger "spread" and are thus closer together on the
  ##screen. Another is that the smaller stimuli are less
  ##distinguishable // the local motion is not as strong somehow. I
  ##sispect the latter is more true for subject GB and the first is
  ##more true for DT. But how to model this?

  ##Another note: we clearly seem to have a case for ignoring the
  ##single datapoint from the very highest spatial frequency. (0.05
  ##degrees/visual angle/eccentricity, making it about 6.75 cycles per
  ##degree?) Possibly we might justify this from the
  ##other 2/3 of the data, and/or literature. Modeling might need to
  ##incorporate a spatial frequency sensitivity function.

  save(list=ls(), file=(savefile <- sub('\\..*$', '_variations.Rdata', my.args[[2]])))
  append(prodfile,savefile)
}

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call(psych.fits, as.list(my.args))

}
