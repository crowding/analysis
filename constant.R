#!/usr/bin/env Rscript

#perform regression and bootstrap, and show the PSE for each subject in each condition.
#Further, build Figure 1 using the example data from subject DT and the other pieces of
#the puzzle.

fit.model <- NA;

source("common.manipulations.r")
source("modeling.manipulations.r")
source("programming.R")
my.args <- c("pools/All_constant.Rdata", "pools/All_constant.products")

psych.fits <- function(...) {
  my.args <- list(...)

  library(plyr)
  library(ggplot2)
  library(psyphy)
  library(boot)
  library(utils)
  library(arm)
  library(lmtest)

  prodfile <- file(my.args[[2]], open='w')
  
  tryCatch(finally=close(prodfile), {

  ##{{{ load the file, do basic manipulations, calculate pCorrect for responses in window

  load(my.args[[1]])
  
  common.manipulations(environment())

  condition.columns <- c('trial.motion.process.radius', 'subject')
  
  trials <-
    within(trials,
           target.spacing <- 2 * pi * trial.motion.process.radius / trial.extra.nTargets)
  trials <- subset(trials, trial.version__.function == "ConcentricTrial")
  trials <- transform(trials, log.target.spacing=log(target.spacing), target.spacing=NULL)

  #strip away data/columns we aren't planning on using, for the time being.
  trials <- subset(trials, select=c(condition.columns,
                             'result.success', 'log.target.spacing', 'motionCondition',
                             'responseInWindow', 'responseTime', 'minResponseTime',
                             'maxResponseTime', 'correct', 'runs.i'))
  rm(triggers)
  rm(runs)
  rm(frame.skips)

  ##Calculate proportion correct among trials where the subject
  ##answered within the permitted time bounds.
  pCorrect <-
    ddply(subset(trials, responseInWindow & !is.na(responseTime)),
          c('motionCondition', 'log.target.spacing', condition.columns),
          function(x) c(n = nrow(x), pCorrect=mean(x$correct)))

  ##}}}

  ##{{{ exploratory raw data plots.

  ##Plot this raw data for the incongruent-motion trials. We're
  ##excluding GB beacuse it turns out we only tested GB in QUEST mode
  ##(quite an inconsistent subject, too.
  alt.data.plot <-
    (ggplot(subset(pCorrect, subject != 'gb'),
           aes(log.target.spacing,
               pCorrect,
               colour = factor(subject)
           )) + geom_point() + geom_line()
     + opts(aspect.ratio=0.5)
     + facet_grid(trial.motion.process.radius ~ motionCondition)
     )

  raw.data.plot <- 

ggplot(subset(pCorrect, subject != 'gb'),
           aes(log.target.spacing,
               pCorrect,
               colour = factor(trial.motion.process.radius))
           ) + geom_point() + geom_line() +
             opts(aspect.ratio=0.5) +
               facet_grid(subject ~ motionCondition)

  print.to.pdf(alt.data.plot, "figure_1/alt_data_plot.pdf")
  print.to.pdf(raw.data.plot, "figure_1/raw_data_plot.pdf")

  ##}}}
  
  ##{{{ Which trials are we considering? What conditions do we plot?

  ##Here's a vector that is TRUE where we trials are acceptable (a
  ##response was collected within the deadlines.) We are excluding
  ##subject GB for now.
  considered.trials <- with(trials, subject != 'gb'
                            & (motionCondition=='incongruent')
                            & responseInWindow
                            & !is.na(responseInWindow))
    
  ##Each distinct combination of eccentricity and target density
  ##I am calling a 'condition.' Here's a list of the conditions.
    conditions <-
      ddply(trials[considered.trials,],
            condition.columns,
            function(df) df[1,condition.columns,drop=FALSE])

  ##}}}

  ##{{{ Experimenting with a variable-asymptote fitting solution. 

  source("asymptote.fitting.R")
  #.(trials$subject) <- factor(.)
  trials$subject <- factor(trials$subject)

  ## now, we want the asymptote to vary per subject -- so here are the
  ## places it can vary. Later on when we do the full model we will
  ## want to extend this to the different motion conditions as well.
  asymptote.cases <- ddply(trials[considered.trials,], c("subject"),
                 function(x) data.frame(g=0.05, lam=0.05))

  ##fit a simple model. Different slope for each subject, different intercept for 
  individual.fit.optim <-
    glm.2asym(              
              correct ~ subject *
                        (log.target.spacing + factor(trial.motion.process.radius)),
              data=trials[considered.trials,],
              link.2asym=joined.asymptote(asymptote.cases),
              trace=5)

  scaling.fit.optim <-
    glm.2asym(
              correct ~ subject *
                        (log.target.spacing + trial.motion.process.radius),
              data=trials[considered.trials,],
              link.2asym=joined.asymptote(asymptote.cases),
              trace=5)

  ##Now compute Hessians and simulated draws.
  individual.fit.optim$hessian <- hessian(individual.fit.optim)
  scaling.fit.optim$hessian <- hessian(scaling.fit.optim)

  individual.fit.optim$sim <- sim(individual.fit.optim, 1000)
  scaling.fit.optim$sim <- sim(scaling.fit.optim, 1000)
  ##find the intercepts of each curve with an arbitrary threshold
  ##(75%). This requires finding a curve crossing.

  ##Now how about a dumbest-possible model -- all psychometric
  ##functions completely independent of one another.
  total.trials <- trials[considered.trials,]
  total.trials$trial.motion.process.radius <- factor(total.trials$trial.motion.process.radius)
  total.cases <- ddply(trials[considered.trials,], c("subject", "trial.motion.process.radius"),
                       function(x) data.frame(g=0.05, lam=0.05))

  total.fit.optim <-
    glm.2asym(correct ~ subject * log.target.spacing * trial.motion.process.radius,
              data=trials[considered.trials,],
              link.2asym=joined.asymptote(total.cases),
              trace=5)
  ##here I must ask whether it's possible to zero the off diagonal
  ##entries that you know to be zero? Otherwise this takes several hours.
  total.fit.optim$hessian <- hessian(total.fit.optim)
  total.fit.optim$sim <- sim(total.fit.optim, 1000)



  ##this is occasionally necessary as i fix on the link function
  0 && {
    scaling.fit.optim$link.2asym <- scaling.fit.optim$link.2asym$relink()
    individual.fit.optim$link.2asym <- individual.fit.optim$link.2asym$relink()
  }

  ##}}}

  ##{{{ plot the psychometric functions resulting from these two fits.

    ##first, predict all curves.
    trials.considered <- trials[considered.trials,]
    plot.points <- ddply(trials.considered, condition.columns, with,
                         data.frame(log.target.spacing =
                                    seq(min(log.target.spacing) - 2,
                                        max(log.target.spacing) + 2, len=100)))

    ##now plot all predictions.
    (ggplot(cbind(plot.points,
                  correct=predict(individual.fit.optim, plot.points, type="response")))
     + aes(log.target.spacing, correct, color=factor(trial.motion.process.radius))
     + facet_grid(subject ~ .)
     + geom_line()
     )

    ##now make an ensemble of 100 predicted curves. We don't need 1000.
    individual.ensemble <- predict.ensemble(individual.fit.optim, which=1:100,
                                            response.name="correct",
                                            plot.points, type="response", .progress="text")
    scaling.ensemble <- predict.ensemble(scaling.fit.optim, which=1:100,
                                         response.name="correct",
                                         plot.points, type="response", .progress="text")
    total.ensemble <- predict.ensemble(total.fit.optim, which=1:100,
                                         response.name="correct",
                                         plot.points, type="response", .progress="text")

    ##and plot the whole bloody ensemble!
    (ggplot(total.ensemble)
     + aes(log.target.spacing, correct,
           color=factor(trial.motion.process.radius),
           group=interaction(trial.motion.process.radius, sim))
     + facet_grid(subject ~ .)
     + geom_line(alpha = 0.1)
     + theme_bw()
     )

    ##how about a ribbon plot of the whole ensemble?     

    ##}}}
    
  ##now to start with the modeling. let's use a logistic psychometric
  ##function with a presumed guess rate of 0.05. The psyphy package
  ##provides this link function 'logit.2asym' to use with R's
  ##generalized linear modeling functions.  (when we do this properly
  ##we will have to fit the guess rate as well, see the psignifit paper
  ##\cite{Wichmann:2001kx}, and the docs for psyphy package) It seems
  ##we will need to fit the guess rate, because it is different for
  ##different subjects...
  fam <- binomial(link=logit.2asym(0.05, 0.05))
  
  ##here's the model fitting function: It allows you to specify a
  ##formula and a subset of trials to use.
  fit.model <<- function(data, subset=NULL, formula=fmla) {
    ##Workaround for glm's nonstandard evaluation discussed here:
    ##https://stat.ethz.ch/pipermail/r-help/2008-February/154201.html
    eval(bquote((glm(formula, family=fam, data=data, subset=.(subset)))))
  }

    ##When we bootstrap the models, we want to compute the PSE and slope
    ##estimates at each of the original conditions. The 'boot' utility
    ##requires a function to compute the desired bootstrap statistics.
    
    counter <- 0 ## for progress bar
    pb <- NULL

    boot.stats <- function(data, indices=!logical(nrow(data)), do.unlist=TRUE, formula=fmla) {
      model <- fit.model(data, subset=indices, formula=formula)
      conditions <- find.intercept(
                                   model, conditions,
                                   'log.target.spacing', response=0.5,
                                   response.name='pCorrect')
      counter <<- counter + 1
      setTxtProgressBar(pb, counter)
      if (do.unlist) {
        unlist(conditions[,c("log.target.spacing", "slope")])
      } else {
        conditions[,c("log.target.spacing", "slope")]
      }
    }

    ##Function to compute the fit and bootstrap fits (random-x, resampled-with-replacement)
    fit.and.boot.model <- function(fmla, data=trials[considered.trials,],
                                   iter=10, cond=conditions) {
      pb <<- txtProgressBar(min=0, max=iter)
      tryCatch(finally=close(pb), {
        counter <<- 0

        model <- fit.model(data, formula=fmla)
        cond <- find.intercept(model, cond, 'log.target.spacing',
                               response=0.5, response.name='pCorrect')
        
        fit.boot <- boot(data, boot.stats, iter,  formula=fmla)
        sds <- apply(fit.boot$t, 2L, sd)
        cond[,c('log.target.spacing.sd', 'slope.sd')] <- sds
        list(model=model, conditions=cond, boot=fit.boot)
      })
    }

    fit.and.simulate.model <- function(fmla, data=trials[considered.trials,], iter=200, cond=conditions) {
      fit.and.simulate(fmla, data=data, iter=iter, cond = conditions)
    }
    
    ##Function to plot the bootstrap estimates in errorbars
    
    plot.errorbars <- function(fit) {
      (ggplot(fit$conditions,
              aes(trial.motion.process.radius, exp(log.target.spacing), color=subject)) 
       + geom_point(shape=1)
       + geom_errorbar(aes(ymin=exp(log.target.spacing - log.target.spacing.sd),
                           ymax=exp(log.target.spacing + log.target.spacing.sd)),
                       width=0.1)
       + scale_x_log(breaks=x <- unique(fit$conditions$trial.motion.process.radius), labels=x)
       + scale_y_log(limits=c(0.5, 8), breaks=1:8, labels=1:8)
       )
    }

    ## Now really starting with the exploration. As a baseline we fit a
    ## logistic curve to each trial condition independently (treating eccentricity as a factor)
    individual.fmla <- correct ~ log.target.spacing*factor(trial.motion.process.radius)*factor(subject)

    ##since resampling bootstrap is slow as hell, and parameteric boot
    ##works fine, I have disabled resampling bootstrap for production.
#    individual.fit <- fit.and.boot.model(individual.fmla, iter=200)
    individual.fit2 <- fit.and.simulate.model(individual.fmla, iter=200)

                                        #let's compare the bootstrap and simulation methods...
##    print(plot.errorbars(individual.fit)) #########################################PAGE 2
    print(plot.errorbars(individual.fit2)) ########################################PAGE 3

    ##we see that the calculated SD's are similar except for the bad one for
    ##SK at the smallest eccentricity.
    
    ##Now the next formula tries to fit with critical distance and
    ##eccentricity accoring to a power law.

    scaling.fmla <- (correct ~ factor(subject)
                     + log.target.spacing:factor(subject)
                     + log(trial.motion.process.radius):factor(subject) )
    scaling.fit <- fit.and.simulate.model(scaling.fmla, iter=200)
    
    ribbon.plot <- function(indiv.fits, model.fits) {
      (plot.errorbars(indiv.fits)
       + geom_ribbon(data = model.fits$conditions,
                     aes(ymin=exp(log.target.spacing-log.target.spacing.sd),
                         ymax=exp(log.target.spacing+log.target.spacing.sd),
                         fill=subject),
                     alpha=0.2, linetype=0)
       + geom_line(data=model.fits$conditions)
       )
    }

    plot.errorbars(individual.fit2) + geom_ribbon(data = scaling.fit$conditions,
                     aes(ymin=exp(log.target.spacing-log.target.spacing.sd),
                         ymax=exp(log.target.spacing+log.target.spacing.sd),
                         ))

    
    print(ribbon.plot(individual.fit2, scaling.fit)) ##################################PAGE 4

    ## We can see there are a couple of points that don't fit well -- sk
    ## at the smallest eccentricity tested, and sm at the
    ## largest. Additionally we didn't make a good fit for any of the
    ## data from gb (but that may be because that data was from QUEST.)
    ## Let's look at whether these subjects were doing teh right thing
    ## by looking at the congruent and counterphase motion cases.
    diag <- ddply(pCorrect, c('motionCondition', condition.columns),
                  function(m) c(n=sum(m$n), pCorrect=sum(m$pCorrect * m$n)/sum(m$n)))

    print(ggplot(diag, aes(trial.motion.process.radius, pCorrect, color=factor(subject))) + facet_grid(. ~ motionCondition) + geom_line())
    ## Some of these response rates look rather poor for these subjects :( 
    
    ## Although the data fit a linear scaling model reasonably well,
    ## there are notable deviations at one end of the scale or the other
    ## for some subjects, for instance SK at the smallest eccentricity
    ## and SM at the largest. Examination of the raw data shows that
    ## these subjects deviated from perfect behavior in the congruent
    ## and counterphase motion conditions, and there are some patterns to
    ## that behavior.
    
    print(ggplot(subset(pCorrect, subject != 'gb'),
                 aes(log.target.spacing-log(trial.motion.process.radius),
                     pCorrect,
                     colour = factor(trial.motion.process.radius)))
          + geom_point()
          + opts(aspect.ratio=0.5)
          + facet_grid(subject ~ motionCondition)
          )                    ############################## PAGE 5

    ## SM seems more easily fooled by incongruent motion at large
    ## eccentricities, but has more trouble with counterhase and
    ## congruent motion at smaller eccentricities. There appears to be
    ## more trouble with counterphase motion at the smaller target
    ## spacings (reasonable), but more trouble with congruent motion at
    ## middling eccentricities (interesting) Perhaps he has a slight
    ## case of what GB had, which was that spatial or temporal
    ## frequenceis were q bit high at the low eccentricities. This would
    ## lead to local motion having a 'weaker' influence on the
    ## combination of cues.

    ## For SM we see a similar pattern: more trouble at lower
    ## eccentricities with congruent motion, more touble at
    ## larger. "Stronger" congruent motion affects the measurement of
    ## PSE (PSE is a bit of a misnomer in the case of this experiement,
    ## because percieved global and local components, phemonenologically
    ## speaking, 'equivalent.' It is, rather where the local and global
    ## components are equally 'strong' in terms of determining the
    ## subject's response. Where is this 'strength' determined?

    ## So the performance in ambivalent and congruent motion may explain
    ## deviations from linear scaling in the case of incongruent
    ## motion. I must start to think about models that take into account
    ## the relative 'strengths' of local and global motion for each
    ## subject at each eccentricity. Data from the control experiments
    ## where I vary temporal and spatial frequency may help with
    ## this. Return to this question later but keep in mind -- we have
    ## this information from 2/3 of the trials that I haven't been
    ## looking at, but could be put to some good use.

    ## Note that for counterphase motion, the logistic fit that goes
    ## from 0% correct to 100% correct makes no sense -- you'de be going
    ## from 50% to 100%. Perhaps the congruent cases would have a
    ## different guessing rate. Need to find out how to account for that
    ## in the model (starts to make it sound like a nonlinear regression)
    
    ## As another interesting thing about this data, let's look at how
    ## the responses are affected by response time. Note this includes
    ## trials that were outside the response window (the subjects were
    ## receiving feedback about whether their responses came early or
    ## late.)
  
    response.time.plot <-
      (ggplot(subset(trials, motionCondition=='incongruent'
                     & !is.na(responseTime) & subject == "dt"),
              aes(log.target.spacing-log(trial.motion.process.radius),
                  responseTime, color=correct))
       + geom_point(position=position_jitter(width=0.02, height=0))
       + facet_grid(trial.motion.process.radius ~ subject)
       + scale_y_continuous(limits=c(0.3, 1.0))
       )

    print(response.time.plot) ############################## PAGE 6

    ## It appears (for DT's responses on ambiguous trials) that she is
    ## more likely to respond according to the LOCAL motion, but when
    ## responding later in the window. Also notice that DT's responses
    ## are neither earlier, nor later, but _more well timed_ when the
    ## stimulus is very easy (the widest motion condition, on the
    ## right.)  Interesting effect that might be outside our scope for
    ## this paper.

    ## Note that the data from the smallest eccentricity condition, which
    ## previously looked grotty and gave a psychometric function with a
    ## wide slope. Let's look at different conditions...

    print(response.time.plot %+% subset(trials, motionCondition=='incongruent' & subject=='sm'))
################################ PAGE 7

    print(response.time.plot %+% subset(trials, motionCondition=='incongruent' & subject=='sk'))
################################ PAGE 8

    print(response.time.plot %+% subset(trials, motionCondition=='incongruent' & subject=='pbm'))
################################ PAGE 9

    ## Note that I don't have much of a response time effect. This may
    ## be experimenter-contingent bias: I probably tuned the stimulus to
    ## work best for me. Note also that 'oops' trials seem to come late in the trial, and that 
    
    ## SM's response time effect goes the opposite way from DT's! It
    ## also may be different at different eccentricities.  The
    ## interesting thing about this is that a psychometric function
    ## whose slope looks pretty shallow, in terms of response rate as
    ## a function of target spacing, instead is going to look much
    ## sharper when the response time is taken into account. What I do
    ## here to demonstrate this is to "normalize" the PSE to a
    ## constant response time response time, in effect saying 'what
    ## would the model say the PSE was, if each subject responded
    ## EXACTLY at 650ms on each trial?

    conditions$responseTime <- 0.650
    
    response.time.conditions.fmla <-
      (correct ~ log.target.spacing*factor(trial.motion.process.radius)*factor(subject)
       + responseTime:factor(subject))
  
    response.time.conditions.fit <-
      fit.and.simulate.model(response.time.conditions.fmla, iter=200,
                             data=subset(trials, motionCondition=='incongruent'
                               & !is.na(responseTime) & subject != "gb"))

    response.time.scaling.fmla <- (correct ~ factor(subject)
                                   + log.target.spacing:factor(subject)
                                   + log(trial.motion.process.radius):factor(subject)
                                   + responseTime:factor(subject))
    
    response.time.scaling.fit <-
      fit.and.simulate.model(response.time.scaling.fmla, iter=200,
                             data=subset(trials, motionCondition=='incongruent'
                               & !is.na(responseTime) & subject != "gb"))

    ribbon.plot(response.time.conditions.fit, response.time.scaling.fit)
    ##################################PAGE 10

    conditions$responseTime <- NULL
    ## This is interesting because applying the correction for response
    ## time actually brought SM's data more in line. What happened to
    ## SK though?

    ## Looking through the fitted data one sees that addng the response
    ## time parameter made the estimates of slope slightly higher and
    ## more consistent with each other. Interesting.

    ## I start to wonder about things like the conduction
    ## velocity of lateral connections in (what area of?) cortex. There is some
    ## interesting psychophysics related to conduction velocity in
    ## regard to collinear facilitation (can't find the reference at
    ## the moment. It was detection of contrast increment in a
    ## rotating Gabor patch flanked by two facilitating Gabor
    ## patches, or somethign along those lines. The facilitation
    ## happened at a particular angle = delay which covaried with the
    ## flanker distance.)

  })

  ## Now having played with the data, output graphs that will go into the paper
  postscript(file="figure_1/c.eps", width=8, height=4)
  tryCatch(finally=dev.off(), {
    writeLines("figure_1/c.eps", prodfile)

    ## the other ones were 4 inches wide, I'll make this 8 by
    ## 4... (what were the font sizes?)

    #make predicitons for the data coming over, for each subject at
    #each eccentricity
    raw.data <-
      ddply(trials[considered.trials,],
            c('motionCondition', 'log.target.spacing', condition.columns),
            function(x) c(n = nrow(x), pCorrect=mean(x$correct)))

    ## now tweak the formatting and labels:

    ## black and white theme
    ## The y axis should run from 0 to 1.
    ## Mike doesn't like the border of the graph lying outside 0,1;
    
    print(example.plot
     + theme_bw(base_size=12)
     + scale_y_continuous(limits = c(0,1), name="Global responses", formatter = "percent")
     + scale_x_log10(name="Element spacing (deg)",
                     breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                     labels = c("",   "",  "",  "",  "", 1, 2,"","", 5,"","","","", 10 ))
     + scale_colour_hue("eccentricity",
                        labels=format(unique(raw.data$trial.motion.process.radius),
                          digits=2, nsmall=2),
                        breaks=levels(factor(raw.data$trial.motion.process.radius)))
     + opts(panel.grid.major = theme_blank(),
            panel.grid.minor = theme_blank()#,
            #panel.border = theme_blank() #looks ugly
            )
          )
  })

  ## check that some conditions are BAD, in that the subjects scored
  ## poorly in ambivalent and congruent conditions...
  
  ## we're going to justify disregarding a problematic data point
  ## that can't be fit.  let's try to regress each condition
  ## separately, and see how significant the slope term is. I think
  ## that's a good check on whether the ratio makes sense. THis is
  ## conceptually equivalent to the individual.fits model, but when
  ## I want to check the fit of each separately...

  ## oh frakking nonstandard evaluation, this invodation of ddply
  ## works outside of trycatch but not inside!! so instead of
  ##goodness <- ddply(conditions, .(1:nrow(conditions)), function(r) {
  ## I have to
  conditions$i <- 1:nrow(conditions)
  goodness <- ddply(conditions, "i", function(r) {
    r$motionCondition <- 'incongruent'
    r$fit <- list(glm(correct ~ log.target.spacing, family=fam, data=merge(r, trials[considered.trials,])))
    r$pval <- coeftest(r$fit[[1]])[2, 4]
    r
  })
  ##ugh, well, that didn't tell me much, of course the slope was significant...
  ##Now I want to look at how overdispersed these fits are.
  
  ##Now we produce a final version of the scaling plot. that I currently call figure 1d.
  pdf(file="figure_1/d.pdf", width=8, height=8)
  tryCatch(finally=dev.off(), {
    writeLines("figure_1/d.pdf", prodfile)

    ## I'm going to exclude the SK data because the data was collected
    ## before methods were finalized. So fixation was not enforced,
    ## and response time was not enforced. Oh well...
    fig.1d <- (ggplot(subset(individual.fit2$conditions, subject != 'sk'),
                      aes(trial.motion.process.radius,
                          exp(log.target.spacing),
                          color=subject))
               + geom_point(shape=1)
               + geom_errorbar(aes(ymin=exp(log.target.spacing - log.target.spacing.sd),
                                   ymax=exp(log.target.spacing + log.target.spacing.sd)),
                               width=0.05)
               + geom_ribbon(data=subset(scaling.fit$conditions, subject != 'sk'),
                               aes(ymin=exp(log.target.spacing-log.target.spacing.sd),
                                 ymax=exp(log.target.spacing+log.target.spacing.sd),
                                 fill=subject),
                             alpha=0.2, linetype=0)
               + geom_line(data=subset(scaling.fit$conditions, subject != 'sk'))
                     )

    print(fig.1d
          + theme_bw(base_size=12)
          + scale_x_log(name="eccentricity (deg)", breaks=x <- unique(
                          individual.fit2$conditions$trial.motion.process.radius),
                        labels=format(x, digits=2, nsmall=2))
          + scale_y_log(name="Element spacing at PSE (deg)", limits=c(0.5, 8), breaks=1:8, labels=1:8)
          + opts(panel.grid.major = theme_blank(),
                 panel.grid.minor = theme_blank()
                 )
          )
    
    })

  savefile <- sub('\\..*?$', ".calcs.Rdata", my.args[[2]])
  save(list=ls(), file=savefile)
  writeLines(savefile, prodfile)
})
}

######

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call(psych.fits, as.list(my.args))
}
