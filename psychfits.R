#!/usr/bin/env Rscript
source("common.manipulations.r")

psych.fits <- function(...) {
  my.args <- list(...)

  library(plyr)
  library(ggplot2)
  library(psyphy) #for glm.lambda, logit.2asym, etc.
  load(my.args[[1]])

  products <- file(my.args[[2]])
  tryCatch(finally = close(products), {
    #do the fit with a 5% adjustment (guessing rate) when fitting with
    #a bigger data set including interactions (not individul points),
    #may be useful to estimate lambda using glm.lambda
    fam <- binomial(link=logit.2asym(0.05, 0.05))

    ##for either constant or varying, give the conditions.
    conditions <- what.varies(runs)
    common.manipulations(environment())

    trials <- within(trials, target.spacing <- 2 * pi * trial.motion.process.radius / trial.extra.nTargets)
    trials <- subset(trials, trial.version...function == "ConcentricTrial")

    ##perform some psychometric fits! we would like to fit a logistic to
    ##the data from EACH condition.  Then we would like to fit a
    ##multiple regression using the multiple parameters that were varied in each condition.

    ##fit logistic functions that scale with radius, having one fit per
    ##radius. Perhaps I ought to do this the nested regression way, with binary
    ##dummy variables one per condition.
    individual.condition.fits <- ddply(trials,
                            c("trial.motion.process.radius", "trial.version...function", conditions),
                            function(group) {
                              data.frame(
                                fit=I(list(glm(correct ~ target.spacing,
                                  family=fam,
                                  group,
                                  subset=with(group,
                                    motionCondition=="incongruent" & responseInWindow)
                                  ))))
                            })

    ##fit functions scaling, having one fit for each condition under
    ##each set of radii. Perhaps this, too would be better with dummy
    ##predictors.
    scaling.fit <- ddply(trials,
                         c("trial.version...function", conditions),
                         function(group) {
                           data.frame(
                             scaling.fit=I(list(glm(
                               correct ~ I(target.spacing/trial.motion.process.radius),
                               family=fam,
                               data=group,
                               subset=with(group,
                                 motionCondition=="incongruent" & responseInWindow)
                               ))))
                         })

    #figure 1a, show overlaid: 1. the raw data for this subject as a
    # function of radius, as points. Show a different plot for each
    # condition.
    pCorrect <- ddply(trials[trials$responseInWindow,],
                      c("motionCondition", "trial.motion.process.radius", "target.spacing", conditions),
                      function(x) c(n = nrow(x), pCorrect=mean(x$correct)))

    a <- ggplot(subset(pCorrect, motionCondition=="incongruent"),
                aes(log(target.spacing), pCorrect, colour=factor(trial.motion.process.radius))) + geom_point() + opts(aspect.ratio=0.5)

    if (length(conditions) > 0) {
        facet.str <- do.call(paste, as.list(c(conditions, sep=" ~ ")))
        if (length(conditions) == 1) {
          facet.str <- paste(facet.str, ".", sep=" ~ ")
        }
        a <- a + facet_grid(facets=as.formula(facet.str))
    } else {
      facet.str <- ""
    }
    ## 2. for each individual condition fit, make predictions from the
    ## fit. Then plot them on top of the next layer.
    ## Question: why doesn't adply work here?
    ## Also, I should be able to just curve()... but can't? hmm.
    predictions <- as.data.frame(do.call(rbind, apply(individual.condition.fits, 1,
                         function(row) {
                           fit <- row$fit
                           row$fit <- NULL
                           row$target.spacing <- seq(
                                                 min(fit$data$target.spacing)/1.5,
                                                 max(fit$data$target.spacing)*1.5,
                                                 len=100)
                           x <- data.frame(row)
                           x$pCorrect <- predict(fit, newdata=x, type="response")
                           x
                         })))

    a <- a + geom_line(data=predictions)
    
    ##predict using each scaling fit (in the case there are several)...
    merge.right <- scaling.fit
    merge.right$scaling.fit <- NULL
    merge.right$scaling.fit.ix <- seq(nrow(scaling.fit))
    predictions <- merge(predictions, merge.right)

    predictions <- ddply(predictions, "scaling.fit.ix", function(group) {
      group$pCorrect.scaling <- predict(
        scaling.fit[[group$scaling.fit.ix[[1]],"scaling.fit"]],
        newdata=group, type="response")
      group
    })

    ##dashed curves show fits constrained to scaling proportional to proportion correct
    a <- a + geom_line(aes(y=pCorrect.scaling), data=predictions, linetype="dashed")
    ##top it off with the subject name in the title... FIXME
##    a <- a + opts(title=paste("Subject", toupper(runs$beforeRun[[1]][["params",1,1]][["subject",1,1]][[1]]), facet.str))

      a <- a + opts(title=paste("Subject", toupper(runs$beforeRun.params[[1]][["subject",1,1]][[1]]), "\n", facet.str))

    #Now begin a new plot showing the PSEs and their standard errors
    
    #write the ggplot to a PDF and export the plot. need a way to
    #automate the product-listing...?
    filename <- sub(".Rdata", "_fits.pdf", my.args[[1]])
    cat(file=products, filename, "\n")

    pdf(file=filename)
    tryCatch(finally = dev.off(), {
      print(a)
    })
  })
}

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call(psych.fits, as.list(my.args))
}
