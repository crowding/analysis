#!/usr/bin/env Rscript
source("common.manipulations.r")
my.args <- c('pools/DT_constant.Rdata', 'test.categorical.pdf')

psych.fits <- function(...) {
  my.args <- list(...)

  library(plyr)
  library(ggplot2)
  library(psyphy)

  load(my.args[[1]])

  products <- file(my.args[[2]])
  tryCatch(finally = close(products), {
    
    common.manipulations(environment())
    trials <-
      within(trials,
             target.spacing <- 2 * pi * trial.motion.process.radius / trial.extra.nTargets)
    trials <- subset(trials, trial.version__.function == "ConcentricTrial")

    #we could work with log(target.spacing) as an expression, but this
    #is easier to pull out slope and intercept per condition.
    trials <- transform(trials, log.target.spacing=log(target.spacing), target.spacing=NULL)

    ##start with a basic data plot to show what the data look
    ##like. First, compute the proportion correct within trial groups:
    pCorrect <-
      ddply(trials[trials$responseInWindow,],
            c('motionCondition', 'trial.motion.process.radius', 'log.target.spacing'),
            function(x) c(n = nrow(x), pCorrect=mean(x$correct)))

    #the base plot
    data.plot <-
      ggplot(subset(pCorrect, motionCondition=='incongruent'),
             aes(log.target.spacing,
                 pCorrect,
                 colour = factor(trial.motion.process.radius))) + 
                   geom_point() +
                     opts(aspect.ratio=0.5)

    ##just to show what it looks like
    quartz()
    print(data.plot);

    ##now to start with the modeling. let's use a logistic
    ##psychometric function with a presumed guess rate of 0.05. The
    ##psyphy package provides this link function 'logit.2asym' to use
    ##with R's generalized linear modeling functions.  (when we do
    ##this properly we will fit the guess rate as well, see the
    ##psignifit paper, and the docs for psyphy package).
    fam <- binomial(link=logit.2asym(0.05, 0.05))

    ##we start by fitting each condition (radius) separately: this
    ##means treating the radius as a factor and expanding it against
    ##the radius (interaction terms allow changes in slope). We only
    ##use the data from incongruent conditions, here.

    ##here we are computing a separate function for each radius.
    conditionKeys <- 'trial.motion.process.radius'

    incongruent.conditions.model <-
      glm(correct ~ log.target.spacing*factor(trial.motion.process.radius),
          data=trials,
          family=fam,
          subset=(motionCondition=='incongruent' & responseInWindow))

    #it'll be useful right here to have a list using each experiment
    #condition we want a curve/threshold for.
    incongruent.conditions <-
      ddply(subset(trials, motionCondition=='incongruent'),
            conditionKeys,
            function(df) {
              data.frame(df[1,conditionKeys,drop=FALSE],
                         min.spacing=min(df$log.target.spacing),
                         max.spacing=max(df$log.target.spacing))
            })
    
    #New let's plot the fitted curves on top of the data. First
    #generate predicted responses from the data for each condition:
    curves <-
      adply(incongruent.conditions, 1,
            transform, log.target.spacing = seq(min.spacing-0.5,max.spacing+0.5,len=100))
    curves$pCorrect <- predict(incongruent.conditions.model, newdata=curves, type="response")

    ##the earlier graph, with fit lines superposed
    print(data.plot + geom_line(data=curves))

    ##now let's compute the PSE estimate. Note that this amounts to
    ##finding the x-intercept of the regression line. Here's a function to do it...

    findIntercept <- function(model, cases, varname, response=0.5,
                              test.values=c(1,2),
                              response.name="response", slope.name="slope") {
      #since there's a lot of interaction terms I'm backing out, i
      #extract the effective intercept and slope for each subgroup
      #with this kludge.
      yval <- model$family$linkfun(response)
      
      cases[, varname] <- test.values[[1]]
      link1 <- predict(model, newdata=cases, type="link")
      cases[, varname] <- test.values[[2]]
      link2 <- predict(model, newdata=cases, type="link")

      slope <- (link2-link1)*(test.values[[2]]-test.values[[1]])
      xintercept <- test.values[[1]] - (link1-yval)/slope
      cases[,varname] <- xintercept
      cases[,response.name] <- response
      cases[, slope.name] <- slope
      cases
    }

    incongruent.conditions <-
      findIntercept(incongruent.conditions.model,
                    incongruent.conditions,
                    'log.target.spacing', response=0.5, response.name='pCorrect')
    
    ##Plot to verify that we found the PSEs correctly (they ought to
    ##lie on the curves at pCorrect = 0.50...
    print(data.plot
          + geom_line(data=curves)
          + geom_point(data=incongruent.conditions, shape=1, size=5)
          )
    
    ## now how do we find some measure of the error in the PSE? This
    ## is, essentially, the 'calibration problem' in regression. There
    ## are various approaches to solving (fiducial intervals, delta
    ## method) but the bootstrap is conceptually easiest.  let's try a
    ## bootstrap.

    library(boot)

    ## do we want to do random resampling (selecting cases at random
    ## with replacement) or model-based resampling (selecting
    ## identical numbers of each predictor?) With a large number of
    ## conditions it seems we would want the model-based boot? Boot by
    ## default does a Another interesting option is a simulation-based
    ## boot (i.e. drawing randomly not form the data but from the
    ## implied multivariate gaussian distribution that was fit by the
    ## GLM.)

    ## In any case, to do a bootstrap we first need a function that
    ## computes the statistic(s) we want: PSE and slope.
    bootStats <- function(data, indices=NULL, do.unlist=TRUE) {
       model <- glm(
                    correct ~ log.target.spacing*factor(trial.motion.process.radius),
                    data=if (is.null(indices)) data else data[indices,],
                    family=fam)
       conditions <- findIntercept(
                                   model, incongruent.conditions,
                                   'log.target.spacing', response=0.5,
                                   response.name='pCorrect')
       if (do.unlist) {
         unlist(conditions[,c("log.target.spacing", "slope")])
       } else {
         conditions[,c("log.target.spacing", "slope")]
       }
    }

    incongruent.conditions.boot <-
      boot(subset(trials, (motionCondition=='incongruent') & responseInWindow),
           bootStats, 1999)

    sds <- apply(incongruent.conditions.boot$t, 2L, sd)
    incongruent.conditions[,c('log.target.spacing.sd', 'slope.sd')] <- sds

    ##now, let's make the plot with the standard deviations of the PSE
    ##statistic overlaid. I can't get geom_errorbarh to work and there
    ##isn't a geom_pointrangeh, so I'll simulate geom_pointrangeh with
    ##points and segments.
    print(data.plot
          + geom_line(data=curves)
          + geom_segment(data=incongruent.conditions,
                         aes(x=log.target.spacing-log.target.spacing.sd,
                             xend=log.target.spacing+log.target.spacing.sd,
                             y=pCorrect, yend=pCorrect))
          + geom_point(data=incongruent.conditions, shape=0)
          )

    ##Looks good. 
    ##now let's load ALL of our subjects together and compute their PSEs for
    ##each condition, as in experiment 1.

    
    
    simple.model <-
      glm(correct ~ log(target.spacing) + log(trial.motion.process.radius),
          data=trials,
          family=fam,
          subset=(motionCondition=='incongruent' & responseInWindow))
    
    ## An interesting question is: how much of this reversal can be
    ## attributed to crowding between targets, versus how much of this
    ## reversal is attributed to just having MORE MOTION in the
    ## correct direction? We can leverage our counterphase data to ive
    ## us information on this.
    
    #now try fitting an offset for each motion condition....
    motion.model1 <- glm(
                        correct ~ log(target.spacing)
                                  + motionCondition
                                  + log(trial.motion.process.radius),
                        data = trials,
                        family = fam,
                        subset = responseInWindow
                        )
    motion.model <- glm(
                        correct ~ log(target.spacing)*motionCondition
                                  + log(trial.motion.process.radius),
                        data = trials,
                        family = fam,
                        subset = responseInWindow
                        )

    
    
})}

