#!/usr/bin/env Rscript

##Analyze the occlusion data, and produce figure 2.

library(plyr)
library(ggplot2)
library(psyphy)
library(boot)
library(utils)
library(arm)
library(lmtest)
library(png)
library(reshape)
library(gmodels)
library(ellipse)
theme_set(theme_bw(base_size=12))
theme_update(opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank()))

fit.model <- NA;

source("common.manipulations.r")
source("modeling.manipulations.r")
script.args <- c("pools/occlusion.Rdata", "pools/occlusion.products")
my.args <- as.list(script.args)
occlusion.analysis <- function(...) {
  my.args <- list(...)

  prodfile <- file(my.args[[2]], open='w')
  tryCatch(finally=close(prodfile), {

    ##{{{ some functions that ought to be exported to a LaTex preamble

    latex.format <- function (x, ...)
    {
      ## format a number to look pretty in latex.
      ## based on 'scinot' in the 'emdbook' package
      y <- strsplit(as.character(format(x, ...)), 
                    "e")[[1]]
      y[1] <- gsub("^0+", "", y[1])
      y[2] <- ifelse(length(grep("^\\+", y[2])) > 0, gsub("^\\+0+", 
                                                          "", y[2]), gsub("^-0+", "-", y[2]))
      ifelse(is.na(y[2]), y[1],
             ifelse(y[1] == "1",
                    paste("10^{", y[2], "}", sep = ""),
                    paste(y[1], "\\\\times 10^{", y[2], "}", sep = "")))
    }

    pretty.pval <- function(p, alpha=0.0001) {
      #format scientific notion as TeX
      if (p < alpha) {
        paste("\\\\ensuremath{p < ", latex.format(alpha), "}", sep="")
      } else {
        paste("\\\\ensuremath{p = ", latex.format(p, digits=2), "}", sep="")
      }
    }

    ## I want to evaluate the "significance" here over a number of
    ## subgroups of the regression. For both of the techniques here, that means:
    ## 1. evaluate the log likelinood of the data for a subset
    ## 2. adjust for the number of degrees of freedom in the model
    ## 2a. adjust for the number of degrees of freedom in the model,
    ##     subject to the blocking that we are under. For example.
    ## 1. calculate the log likelihood among restricted groups. If a
    ## subset was used, you'll have to specify the entries among the
    ## subset.
    chisq.subset <- function (model1, model2, subset=rep(TRUE, NROW(model1$y))) {
      ## perform a chi-squared test on a _subset_ of observations
      ## between two fitted models. The subset is given as a binary
      ## vector.

      ## to calculate the difference in deviance between the two
      ## models, we want the residual deviance among the subsets.
      ## this relplicates GLM's AIC calculation:
      aic <- ldply(list(model1, model2), function(model) {
        y <- model$y ##all this crud so we can call initialize. Ugly!
        nobs <- NROW(y)
        weights <- model$prior.weights
        eval(model$family$initialize) ## ugly corner of the glm code:
                                      ## this sets n in case of count
                                      ## data (versus standard yes-no)
        ## We need to also determine if the subset has lower rank
        rank <- qr(model.matrix(model)[subset,])$rank
        aic <- model$family$aic(y[subset],
                                n[subset],
                                model$family$linkinv(model$linear.predictors[subset]),
                                weights[subset], NULL) + 2*rank
        ll <- -(aic - 2*rank)/2
        df <- NROW(y[subset]) - rank
        c(rank=rank, aic=aic, ll=ll, df=df)
      })

      ## now make a likelihood ratio test test on the appropriate
      ## number of degrees of freedom. this reflects the p with the
      ## first model as null. Note the 2* for a two tailed test.
      null.ix <- which(aic$df == max(aic$df))
      test.ix <- c(1,2)[-null.ix]
      aic$p[test.ix] <- with(aic, 
                             2 * dchisq(-2 * (ll[null.ix] - ll[test.ix]),
                                    df[null.ix] - df[test.ix]))
      aic
    }

    ##}}}

    ##{{{ Loading the file and performing some data cleanup.

    load(my.args[[1]])
  
    common.manipulations(environment())
    
    ##should come up with visibilityCondition unless it's on crack
    condition.columns <- c('trial.motion.process.radius', 'subject', what.varies(runs))

    ##extract some information about the occluders
    trials$occluderColor <- sapply(trials$trial.occluders,
                           function(t) if(is.na(t)) NA else t[[1]][['color',1,1]][1])
    trials$arcAngle <- sapply(trials$trial.occluders,
                              function(t) if(is.na(t)) NA else t[[1]][['arcAngle',1,1]])
    trials$startAngle <- sapply(trials$trial.occluders,
                                function(t) if(is.na(t)) NA else t[[1]][['startAngle',1,1]])
    trials <- join(trials, ddply(trials, .(runs.i),
                                 function(t) c(n.visibilityConditions=
                                               length(unique(t$visibilityCondition)))))
    trials$contrast <- (trials$occluderColor - 127.5) / 127.5
      
    trials <-
      within(trials,
             target.spacing <- 2 * pi * trial.motion.process.radius / trial.extra.nTargets)
    trials <- subset(trials, trial.version...function == "ConcentricTrial")
    trials <- transform(trials, log.target.spacing=log(target.spacing), target.spacing=NULL)

    #fix a goof
    trials$subject[trials$subject == 'GB'] = 'gb'
    trials$subject <- factor(trials$subject) 
    
    ##strip away data/columns we aren't planning on using, for the time being.
    trials <- subset(trials, select=c(condition.columns,
                               'arcAngle', 'startAngle', 'occluderColor',
                               'n.visibilityConditions', 'result.success',
                               'log.target.spacing', 'motionCondition',
                               'responseInWindow', 'responseTime', 'minResponseTime',
                               'maxResponseTime', 'correct', 'runs.i', 'i',
                               'trial.extra.phase', 'trial.extra.globalDirection',
                               'trial.extra.globalVScalar', 'trial.extra.dt',
                               'trial.motion.process.n', 'trial.extra.nTargets'))
    rm(triggers)
    rm(runs)
    rm(frame.skips)

    ##}}}
    
    ##{{{ setting up our regression modeling and graphics parameters.

    if (!("--slave" %in% commandArgs())) { #Rscript...
      quartz()
    }

    ##here's the model fitting function: It allows you to specify a
    ##formula and a subset of trials to use.
    fit.model <<- function(data, subset=NULL, formula=fmla) {
      ##Workaround for glm's nonstandard evaluation discussed here:
      ##https://stat.ethz.ch/pipermail/r-help/2008-February/154201.html
      eval(bquote((glm(formula, family=fam, data=data, subset=.(subset)))))
    }
    counter <- 0 ## for progress bar
    pb <- NULL
    fit.and.simulate.model <- function(fmla, data=trials[considered.trials,],                                       iter=200, cond=conditions) {
      fit.and.simulate(fmla, data=data, iter=iter, cond = cond)
    }
    ## the logistic-with-guessing-rate model
    fam <- binomial(link=logit.2asym(0.05, 0.05))

    ##}}}

    ##{{{ Deciding what stimulus settings to use.

    ##what simulus configurations did I try among my various runs?
    session.occluders <- ddply(trials, .(runs.i),
                               function(t) data.frame(
                                  contrast = unique(abs((t$occluderColor - 127.5)/127.5)),
                                  conditions = unique(t$n.visibilityConditions),
                                  arcAngle = unique(t$arcAngle),
                                  n = nrow(t),
                                  subject=unique(t$subject)))

    trials$contrast <- abs(trials$occluderColor - 127.5)/127.5

    ##only take seriously sessions where we collected 800 before stopping...
    long.runs <- with(session.occluders, runs.i > 800)
    trials <- trials[trials$runs.i %in$ long.runs,]
    
    ##let's fit a simple model ala constant.
    
    #first, have a raw data plot...
    (
      ggplot(subset(trials, motionCondition=='incongruent'),
             aes(log.target.spacing, log(trial.motion.process.radius),
                 color=correct))
     + geom_point(alpha=0.4, position=position_jitter(w = 0, h = 0.05))
      + facet_grid(subject ~ visibilityCondition)
    )

   
    trials <- merge(trials, session.occluders, all.left=TRUE)

    trials <- within(trials, {
       factor(paste(factor(conditions), factor(visibilityCondition),
                            factor(subject), factor(contrast), factor(360 - arcAngle*180/pi)))
      shape <- factor(paste(factor(contrast), factor(conditions), factor(360 - arcAngle*180/pi)))
    })

    considered.trials <- with(trials,
                              (motionCondition=='incongruent')
                              & responseInWindow
                              & !is.na(responseInWindow))

    
    ## which conditions to produce PSE estimates for.
    groups <- function(df, columns) {
      ddply(df, columns, function(d) d[1,columns, drop=FALSE])
    }
    conditions <- groups(trials[considered.trials,], c(condition.columns, 'group', 'shape'))

    ## as justification for the final choice of stumulus configuration, we look
    ## at the effect of occluder visibility and side-switching for
    ## various subjects.

    ## let's have a single slope for each subject, as that seems to
    ## give me reliable data. But calculating different PSEs for each
    ## experiment condition.  since I didn't sample the entire passel
    ## of experiment conditions for each subject, I create a new
    ## factor enumarating the conditions I did try.
    
    justification.fmla <- correct ~ log.target.spacing*subject + factor(trial.motion.process.radius)*group

    ## what happens if we look at a single slope per group, instead of pooling
    ## subjects?
    
    ##now fit and simulate the exploratory data.  this will complain
    ##of rank deficiency because "group" subsumes "subject." But
    ##that's OK for the point estimates, I don't ask it to extrapolate
    ##untested groups. However this does make errorbars invalid :(
    justification.fit <- fit.and.simulate.model(justification.fmla)

    ##now plot the exploratory data. facets for each subject and for
    ##left/right/center, shape code for experiment condition.
    justification.plot <-
      (ggplot(justification.fit$conditions)
       + aes(trial.motion.process.radius,
             exp(log.target.spacing),
             shape=shape)
       + geom_point()
       + facet_grid(visibilityCondition ~ subject)
       + scale_y_log(limits = c(0.6, 10),
                     labels=format(seq(1,8)),
                     breaks=seq(1,8),
                     name="target spacing at PSE")
       + scale_x_log(limits=c(2.5, 12),
                     breaks=x<-unique(conditions$trial.motion.process.radius),
                     labels=format(x, digits=2),
                     name="eccentricity")
       + scale_shape_discrete(name="configuration")
       )

    # the relevant supplementary plot to make excludes the 90 degree
    # data, which happens to be group 5 here, so pull that out of the
    # supplementary figure.
    suppfig.data <- subset(justification.fit$conditions, as.numeric(shape) %in% seq(1,4))
    levels(suppfig.data$shape) <- c("no switching, invisible", "switching, invisible", "no switching, visible", "switching, visible", "")
    
    # close enough. render the figure.
    pdf(file="figure_2/justification.pdf", width=8, height=8)
    tryCatch(finally=dev.off(), {
      writeLines("figure_2/justification.pdf", prodfile)
      print(justification.plot %+% suppfig.data)
    })

    # from this data, we conclude that a visible occluder doesn't help
    # and (comparing full visibility to side/side visibility) the
    # combination of a visible target and side-switching is
    # particularly bad. So for the remainder of our analysis, we use
    # the data that has an invisible occluder and one side per
    # session. FOR NOW this excludes the author; the author needs to
    # re-take this data.
    considered.trials <- with(trials,
                              (motionCondition=='incongruent')
                              & responseInWindow
                              & !is.na(responseInWindow)
                              & (360 - (arcAngle*180/pi) > 100)
                              & contrast < 0.02)

    conditions <- groups(trials[considered.trials,], condition.columns,
                        function(df) df[1,condition.columns,drop=FALSE])

    ##}}}

    ##{{{ looking at each day's data separately.

    ## now the next sode point we want to answer is, did the subjects
    ## answer the same way on each day that they ran the same stimulus configuration?
    ## plot wach stimulus coconfiguration on each day, by adding a factor:
    trials$configuration.daily <- with(trials, factor(paste(runs.i, visibilityCondition)))

    ## look at how the PSEs vary that way...
    daily.conditions <- groups(trials[considered.trials,],
                               c(condition.columns, 'runs.i', 'configuration.daily'))
    daily.fmla <-
      (correct ~ log.target.spacing
       + log.target.spacing:subject
       + configuration.daily*factor(trial.motion.process.radius)
       )
    daily.fit <- fit.and.simulate.model(daily.fmla, iter=1000, cond=daily.conditions)
    ## plot a line for each deal, with symbols

    daily.plot <-
      (ggplot(daily.fit$conditions,
              aes(trial.motion.process.radius,
                  exp(log.target.spacing),
                  color=visibilityCondition,
                  shape=factor(runs.i)))
       + geom_point()
       + geom_line()
       + scale_color_hue('visibility')
       + scale_fill_hue('visibility')
       + geom_ribbon(aes(ymin=exp(log.target.spacing-log.target.spacing.sd),
                         ymax=exp(log.target.spacing+log.target.spacing.sd),
                         fill=visibilityCondition),
                     alpha=0.2, linetype=0)
       + facet_wrap(~ subject, ncol=1)
       + scale_x_log('eccentricity(deg)',
                     breaks=x<-unique(daily.fit$conditions$trial.motion.process.radius),
                     labels=format(x, digits=2))
       + scale_y_log(name='Element spacing at PSE (deg)',
                     breaks=1:10,
                     labels=c(1,2,'','',5,'','','','',10))
       + scale_shape_manual(name="session",
                            breaks=x<-unique(factor(daily.fit$conditions$runs.i)),
                            labels=x,
                            values=seq(length(x)))
       + opts(strip.background = theme_blank(),
              strip.text.x = theme_text(hjust=0))
       )

    pdf(file="figure_2/daily.pdf", width=8, height=8)
      tryCatch(finally=dev.off(), {
        writeLines("figure_2/daily.pdf", prodfile)
        print(daily.plot
              %+% subset(daily.fit$conditions,
                         !(runs.i %in% c(9, 10, 14)))
              + scale_shape_manual(name="session",
                                   breaks=x<-setdiff(sort(unique(factor(daily.fit$conditions$runs.i))), c(9, 10, 12)),
                                   labels=x,
                                   values=seq(length(x)))
              )
      })

    ##}}}

    ##{{{ plotting subfigure A, and the daily version too

    ##Based on this, I am going to use the fully trained data from the
    ##last two sessions, i.e. dropping sessions 9, 10, and 12. The PSE
    ##for full visibility is not appreciably different in those cases.
    
    ## this suggests a better version of figure 2a might be to split
    ## up the daily data, using the kept dataset.

    ##that's interesting. It seems that subject DT did not experience
    ##any enhancement session-to-session (though the performance
    ##shifted a bit between sessions.) BG on the other hand did show
    ##some improvement in the left and right cases versus full. I
    ##wonder if it would be better to quantify this as an
    ##'improvement...' over all radii. oh, foo, that would just be
    ##chasing irregularities in the data.

    ##now let's plot the main figure, with scaling-law fits.

    ## we let the PSE vary arbitrarily per condition but the slope
    ## varies only per subject (and is constant across conditions for each
    ## subject.
    varying.pse.fmla <- (
        correct ~ log.target.spacing
        + visibilityCondition*subject*factor(trial.motion.process.radius)
        + log.target.spacing:subject
    )
    scaling.pse.fmla <- (
        correct ~ log.target.spacing
        + visibilityCondition*subject*log(trial.motion.process.radius)
        + log.target.spacing:subject
    )

    varying.pse.fit <- fit.and.simulate.model(varying.pse.fmla, iter=1000)
    scaling.pse.fit <- fit.and.simulate.model(scaling.pse.fmla, iter=1000)

    #note that these complain about rank-deficiency, but the
    #predictions do not ask to extrapolate outside observed parameter
    #values.
    
    ##I would like now to, within these models, obtain a fit to the
    ##guess rate, psignifit style...  or, in the write up, Wichmann
    ##and Hill style.  \cite{Wichmann:2001kx} And I would like that
    ##guess rate to be fit per subject (but not covarying with
    ##anything else.)  I'm not sure if I can do that in one formula?
    ##Perhaps a nonlinear regression would be required.

    plot.ribbon <- function(fit, fit2) {
      (ggplot(fit$conditions,
              aes(trial.motion.process.radius,
                  exp(log.target.spacing),
                  color=visibilityCondition))
       + geom_point()
       + geom_line(data=fit2$conditions)
       + scale_color_hue('visibility')
       + scale_fill_hue('visibility')
       + geom_errorbar(aes(ymin=exp(log.target.spacing - log.target.spacing.sd),
                           ymax=exp(log.target.spacing + log.target.spacing.sd)),
                       width=0.1)
       + geom_ribbon(data = fit2$conditions,
                     aes(ymin=exp(log.target.spacing-log.target.spacing.sd),
                         ymax=exp(log.target.spacing+log.target.spacing.sd),
                         fill=visibilityCondition),
                     alpha=0.2, linetype=0)
       + facet_wrap(~ subject, ncol=1)
       + scale_x_log('eccentricity(deg)',
                     breaks=x <- unique(fit$conditions$trial.motion.process.radius),
                     labels=format(x, digits=2))
       + scale_y_log(name='Element spacing at PSE (deg)',
                     breaks=1:10,
                     labels=c(1,2,'','',5,'','','','',10))
       + opts(strip.background = theme_blank(),
              strip.text.x = theme_text(hjust=0))
       )
    }
    
    ##start wtih this plot
    a <- (plot.ribbon(varying.pse.fit, scaling.pse.fit))

    #and add the number of trials analyzed in each figure?

    ##somewhat advanced grid graphics hack: modify the legend to
    ##include the tableaux images, and repack into the frame.
    aGrob <- ggplotGrob(a);
    theLegend <- getGrob(aGrob, 'legend.frame', grep=TRUE)
    thePlots <- getGrob(aGrob, 'legend.frame', grep=TRUE)
    mapply(levels(trials$visibilityCondition), 4:6, FUN=function(name, i) {
      thePic <- rasterGrob(readPNG(paste("figure_2/", name, ".png", sep="")),
                           height=unit(3, "lines"))
      theLegend <<- packGrob(theLegend, thePic, col=5, row=i,
                             border=unit(c(0.5, 0.25, 0.25, 0), 'lines'))
    })
    aGrob <- setGrob(aGrob, 'legend.frame', theLegend, grep=TRUE)
    aGrob$childrenvp$parent$layout$widths[[4]] <- grobWidth(theLegend) + unit(0.5, "lines")

    #write the graph to disk
    pdf(file="figure_2/a.pdf", width=4.5, height=8)
    tryCatch(finally=dev.off(), {
      writeLines("figure_2/a.pdf", prodfile)
      grid.newpage();
      grid.draw(aGrob)
    })

    ## But looking back up at the daily data, it may be more
    ## informative to do this while splitting up the
    ## fiull-visibiilty data per session so that we can do the
    ## comparisons within sessions.
    considered.trials <- considered.trials & !(trials$runs.i %in% c(9, 10, 14))
    
    daily.conditions <- groups(trials[considered.trials,],
                               c(condition.columns, 'runs.i', 'configuration.daily'))

    daily.lines.fmla <-
      (correct ~ log.target.spacing
       + log.target.spacing:subject
       + configuration.daily*log(trial.motion.process.radius)
       )
    
    daily.fit <- fit.and.simulate.model(daily.fmla, iter=1000, cond=daily.conditions)
    daily.lines <- fit.and.simulate.model(daily.lines.fmla, iter=1000, cond=daily.conditions)

    #I can't deal with the "sessions" legend and it's kind of unnecessary.
    bPlot <-
           (plot.ribbon(daily.fit, daily.lines)
            + aes(shape=factor(runs.i))
            + scale_shape(name="session",
                          breaks=x<-sort(unique(factor(daily.fit$conditions$runs.i))),
                          labels=x,
                          solid=TRUE,
                          legend=FALSE
                          )
            )

    ##and insert the tableaux into the plot.
    bGrob <- ggplotGrob(bPlot)
    theLegend <- getGrob(bGrob, 'legend.frame', grep=TRUE)
    thePlots <- getGrob(aGrob, 'legend.frame', grep=TRUE)
    mapply(levels(trials$visibilityCondition), 4:6, FUN=function(name, i) {
      thePic <- rasterGrob(readPNG(paste("figure_2/", name, ".png", sep="")),
                           height=unit(3, "lines"))
      theLegend <<- packGrob(theLegend, thePic, col=5, row=i,
                             border=unit(c(0.5, 0.25, 0.25, 0), 'lines'))
    })
    bGrob <- setGrob(bGrob, 'legend.frame', theLegend, grep=TRUE)
    bGrob$childrenvp$parent$layout$widths[[4]] <- grobWidth(theLegend) + unit(0.5, "lines")

    pdf(file="figure_2/a_daily.pdf", width=5.5, height=8)
    tryCatch(finally=dev.off(), {
      writeLines("figure_2/a_daily.pdf", prodfile)
      grid.newpage()
      grid.draw(bGrob)
    })

    ##}}}
    
    ##{{{ bootstrap tests for difference in PSE between conditions.

    ## Now let's do a statistical test for whether the left/right was
    ## greater than the fully visible. This just amounts to counting
    ## up bootstrap runs where one was greater than the other. Er, how
    ## to make a statement across all radii though? isn't that more of
    ## a paired comparison?

    ## Well, let's just count them up by condition first. For each
    ## bootstrap run, compute the difference in log critical spacing
    ## between the "full" visibility and the " (in each session)

    ## match corresponding side conditions with full conditions tested in the same session.
    daily.fit$conditions$i <- seq(nrow(daily.fit$conditions))
    corresponding.conditions <- 
      subset(merge(subset(daily.fit$conditions, visibilityCondition != 'full'),
                   subset(daily.fit$conditions, visibilityCondition == 'full'),
                   by=setdiff(c(condition.columns, 'runs.i', 'trial.motion.process.radius'),
                              'visibilityCondition'),
                   suffixes=c('.occluded', '.full')),
             select=c('i.occluded', 'i.full'))

    ##go through each of our existing bootstrap coefficient
    ##simulations, recording the difference in log critical spacing
    ##for each condition at each bootstrap run.
    print('finding PSE differences between occluded and unoccluded')
    differences <- adply(daily.fit$sim@coef, 1, function(coefs) {
      cbind(daily.fit$conditions[corresponding.conditions$i.occluded,],
            data.frame(
                 difference = (
                      find.intercept(daily.fit$model,
                                     cases = daily.fit$conditions[corresponding.conditions$i.occluded,],
                                     'log.target.spacing',
                                     coef=coefs,
                                     )$log.target.spacing
                      - find.intercept(daily.fit$model,
                                       cases = daily.fit$conditions[corresponding.conditions$i.full,],
                                       'log.target.spacing',
                                       coef=coefs,
                                       )$log.target.spacing
                      )
                 )
            )
      }, .progress='text')

    ##for each occluded condition, count the number of PSEs that were
    ##elevated, and the mean elevation.
    difference.summary <- function(f)
      summarize(f,
                difference.n = length(difference),
                difference.positive = mean(difference > 0),
                difference.mean = mean(difference),
                difference.sd = sd(difference))

    condition.differences <-
      merge(daily.fit$conditions,
            ddply(differences, .(i), difference.summary),
            by='i')

    ##for further simplicity, in the writeup, we also compute a summary of "mean
    ##difference in PSE over all eccentricities."
    ##compute average difference over exentricities for each simulation (X1)
    print('summarizing occlusion differences')
    difference.over.eccentricity <-
      ddply(
            ddply(differences,
                  c('X1','subject','visibilityCondition'),
                  summarize,
                  difference = mean(difference),
                  .progress='text'),
            c('subject', 'visibilityCondition'),
            difference.summary
            )

    ##}}}
                    
    #and there's a hard to interpret set of results. The first thing
    #to notice is that the scaling law is apparent for each
    #experimental condition.  It appears to be the case for subject GB
    #that the left- and right- visible data can have a smaller PSE (test
    #to justify this.) but now there are caveats and alternate
    #explanations that can be driven by the data.

    ##{{{ looking at the ancillary visual cues.

    ##{{{ Calculating facts about the appearances and disappearances.

    #the important point here is that the character of the percept is
    #largely determined by the flanker distance rather than the object
    #countt. If it were object count, then some of these trials --
    #where on average 2 or 3 targets are visible -- would show a
    #dramatic improvement. What this shows is that even in cases where
    #only two or three targets are visible, the target spacing still
    #determines performance. Aha! here is how to talk about it. Take
    #trials where only two or three or four targets are visible, and
    #THEN show that WITHIN these trials, the spacing of the targets is
    #what matters -- and spacing dominates object count. Thus we use
    #the space information!

    #so we need to break down which trials have appearance events and
    #which have disappearance events; and how many blobs are visible
    #at the beginning of each trial. analysis will show that target
    #count gives no purchase over target spacing...

    ##relevant column names:

    ##quantities to look at:
    ##minimum number of targets visible
    ##maximum number of targets visible
    ##targets visible at beginning
    ##targets viisble at end
    ##number of disappearance events
    ##number of appearance events
    ##onset time of first disappearance event or 0 (or NA)
    ##onset time of first appearance event or 0 (or NA)
    
    wrap <- function(x, start, length) (x-start) %% length + start;
      
    cue.calculations <- function(trials) { with(trials, within(list(), {

      phaseSpacing <- exp(trials$log.target.spacing) / trials$trial.motion.process.radius
      phaseStep <- (trials$trial.extra.globalVScalar
                    * trials$trial.extra.dt
                    * sign(trials$trial.extra.globalDirection) )
      
      ##the dataset had the start and size of the occluder, but here I
      ##wrote with the start and end of the window in mind.
      windowStartingPhase <-
        ifelse(trials$visibilityCondition == "full",
               NA,wrap(trials$startAngle + trials$arcAngle, -pi/2, 2*pi))
      windowEndingPhase <-
        ifelse(trials$visibilityCondition == "full",
               NA,
               wrap(trials$startAngle, -pi/2, 2*pi))
      
      ##phase of the first (most clockwise) target visible at the
      ##motion onset.
      firstTargetPhase <-
        wrap(trials$trial.extra.phase, windowStartingPhase, phaseSpacing)
      lastTargetPhase <-
        wrap(trials$trial.extra.phase, windowEndingPhase-phaseSpacing, phaseSpacing)

      #number of targets visible at onset
      onset.visible <- round((lastTargetPhase - firstTargetPhase) / phaseSpacing) + 1

      ##number of appearances and disappearances
      appearances <-
        ifelse(trials$trial.extra.globalDirection > 0,
               (firstTargetPhase - windowStartingPhase
                + (trials$trial.motion.process.n-1) * phaseStep
                ) %/% (phaseSpacing),
               (windowEndingPhase - lastTargetPhase
               - (trials$trial.motion.process.n-1)*phaseStep
                ) %/% (phaseSpacing)
               )
      
      ##number of disappearances
      disappearances <-
        ifelse(trials$trial.extra.globalDirection > 0,
               (windowEndingPhase - lastTargetPhase
                - (trials$trial.motion.process.n-1) * phaseStep
                ) %/% (-phaseSpacing) + 1,
               (firstTargetPhase - windowStartingPhase 
                + (trials$trial.motion.process.n-1) * phaseStep
                ) %/% (-phaseSpacing) + 1
               )
      
      ##time of first appearance
      appearanceTime <-
        ifelse(trials$trial.extra.globalDirection > 0,
               (windowStartingPhase - firstTargetPhase + phaseSpacing)
               / phaseStep * trials$trial.extra.dt,
               (windowEndingPhase - lastTargetPhase - phaseSpacing)
               / phaseStep * trials$trial.extra.dt
               )
      
      ##time of first disappearance
      disappearanceTime <-
        ifelse(trials$trial.extra.globalDirection > 0,
               (windowEndingPhase - lastTargetPhase)
               / phaseStep * trials$trial.extra.dt,
               (windowStartingPhase - firstTargetPhase)
               / phaseStep * trials$trial.extra.dt
               )
      appearances[is.na(appearances)] <- 0;
      disappearances[is.na(disappearances)] <- 0;
      appearanceTime[appearances == 0] <- 0
      disappearanceTime[disappearances == 0] <- 0

      offset.visible <- onset.visible + appearances - disappearances

      ##this is only valid mecause there is never more than one
      ##appearance or disappearance, which I assert by setting any
      ##trials where that happened to NA
      stimulus.duration <- ((trials$trial.motion.process.n-1)*trials$trial.extra.dt)
      mean.visible <- offset.visible + (disappearances * disappearanceTime - appearances*appearanceTime)/stimulus.duration
      mean.visible[appearances >= 2 | disappearances >= 2] <- NA

      ##minimum and maximum number of targets visible at any time during the trial.
      max.visible <- ifelse((appearances > 0 & disappearances == 0) | (appearances > 0 & disappearances > 0 & appearanceTime < disappearanceTime), onset.visible+1, onset.visible)
      min.visible <- ifelse((appearances == 0 & disappearances > 0) | (appearances > 0 & disappearances > 0 & disappearanceTime < appearanceTime), onset.visible-1, onset.visible)

    }))}

    calculations <- as.data.frame(cue.calculations(trials))
    ##FOR A SANITY CHECK, LOOK AT THIS:
    ##ddply(cbind(calculations, trials)[occluded.trials,], c("trial.extra.nTargets", "onset.visible", "appearances", "disappearances", "max.visible","min.visible"), nrow)

    ##}}}

    ##{{{ Exploratory plot shows that density seems to matter even with appearances/ disappearances.

    ##make a plot to illustrate how well distributed the target density
    ##vs. target visibility vs. appearances/disappearances are...
    cases <- ddply(subset(cbind(trials, calculations),
                          considered.trials & visibilityCondition != 'full'),
                   c('subject', 'visibilityCondition', 'onset.visible',
                     'trial.extra.nTargets', 'motionCondition'),
                   summarize,
                   cases = length(i),
                   correct = mean(correct)
                   )

    coverage.plot <-
       (ggplot(cases, aes(trial.extra.nTargets, onset.visible, color=correct, size=cases))
        + geom_point()
        + scale_fill_gradient2(limits=c(0,1),
                                low="red", mid="black", high="green",
                                midpoint=0.5)
        + facet_grid(subject ~ visibilityCondition)
        )
     coverage.plot

    write.pdf <- function(plot, filename, ...) {
      pdf(file=filename, ...)
      tryCatch(finally=dev.off(), {
        writeLines("figure_2/daily.pdf", prodfile)
        print(plot)
      })
    }

    write.pdf(coverage.plot, "figure_2/visible_targets.pdf", width=8, height=8)
    ##Oh, lovely, that is very promising, even in trials with the
    ##same number of visible-at-onset it looks pretty clear that
    ##closer spacings are more likely to result in errors. Whee! But
    ##notice that subject DT got stuck on 2 targets on the right side
    ##a lot? Did that happen with the visible occluder? Is that
    ##reason to use the visible occluder data after all?

    ##}}}

    ##{{{ check my arithmetic with some graphs. Commented out via 1 ||

    1 || {
      subplot <- function(x, y, what) print(what, vp=viewport(layout.pos.row=y, layout.pos.col=x))
      grid.newpage()
      pushViewport(viewport(layout=grid.layout(2,2)))

      ##from top to bottom, go through the calculations veryfying that they look "right"
      qplot(trial.extra.phase, firstTargetPhase, data=cbind(calculations, trials),
            facets = trial.extra.nTargets ~ visibilityCondition, size=1)
      qplot(trial.extra.phase, lastTargetPhase, data=cbind(calculations, trials),
            facets = trial.extra.nTargets ~ visibilityCondition, size=1)
      
      qplot(trial.extra.phase, onset.visible,
            data=subset(cbind(calculations, trials), arcAngle < 4.2),
            facets = trial.extra.nTargets ~ visibilityCondition, geom="line")

      qplot(trial.extra.phase, appearances,
            data=subset(cbind(calculations, trials), arcAngle < 4.2),
            facets = trial.extra.nTargets ~ visibilityCondition, geom="line",
            color=factor(trial.extra.globalDirection))

      qplot(trial.extra.phase, disappearances,
            data=subset(cbind(calculations, trials), arcAngle < 4.2),
            facets = trial.extra.nTargets ~ visibilityCondition, geom="line",
            color=factor(trial.extra.globalDirection))

      calculations$visualGrouping <-
        with(trials,factor(paste(visibilityCondition, trial.extra.globalDirection)))
      calculations$plotVerify <-
        with(cbind(trials, calculations), arcAngle < 4.2
             & visibilityCondition != "full" & trial.extra.nTargets < 16)
      
      ##checking that # appearances and # disappearances should
      ##coincide when nTargets mode 3 == 0
      qplot(trial.extra.phase, disappearances,
            data=subset(cbind(calculations, trials), plotVerify),
            facets = trial.extra.nTargets ~ visualGrouping,
            geom="line") + geom_line(aes(trial.extra.phase, appearances), color="green")
      
      ##compare appearance time to # of appearances.
      qplot(trial.extra.phase, appearanceTime,
            data=subset(cbind(calculations, trials), plotVerify),
            facets = trial.extra.nTargets ~ visualGrouping,
            geom = "line", color=appearances)

      ## and disappearances
      ##compare appearance time to # of appearances.
      qplot(trial.extra.phase, disappearanceTime,
            data=subset(cbind(calculations, trials),plotVerify),
            facets = trial.extra.nTargets ~ visualGrouping,
            geom = "line", color=disappearances)
      
      ##min and max visible, do they make sense vs. appearance and
      ##disappearance times?
      (ggplot(data=subset(cbind(calculations, trials),plotVerify), aes(x = trial.extra.phase))
       + geom_line(aes(y=max.visible-onset.visible), color='black')
       + geom_line(aes(y=min.visible-onset.visible), color='black')
       + geom_line(aes(y=appearanceTime), color='green')
       + geom_line(aes(y=disappearanceTime), color='red')
       + facet_grid(trial.extra.nTargets ~ visualGrouping)
       )
      ##this looks self consistent. we will know that this calculation
      ##happens right when we see that appearances and disappearaces
      ##have an effect.
    }

    ##}}}

    trials <- cbind(calculations, trials)
    ##use this if you have to overwrite those columns.
    ##trials <- trials[,colnames(trials)[-grep('[0-9]$', colnames(trials))]]
    rm(calculations)

    occluded.trials <- considered.trials & (trials$visibilityCondition != "full")

    ##{{{ Look at the effect of appearances and disappearances on proportion correct.

    ##So if I'm right, appearances and disappearances should affect
    ##the proportion correct.
    trials$events <- trials$appearances + trials$disappearances

    pc.e <- ddply(trials[considered.trials,],
                  c("subject", "visibilityCondition",
                    "events", "trial.extra.nTargets"), summarize, pCorrect=mean(correct))
    pc.a <- ddply(trials[considered.trials,],
                  c("subject", "visibilityCondition",
                    "appearances", "trial.extra.nTargets"), summarize, pCorrect=mean(correct))
    pc.d <- ddply(trials[considered.trials,],
                  c("subject", "visibilityCondition",
                    "disappearances", "trial.extra.nTargets"), summarize, pCorrect=mean(correct))

    qplot(trial.extra.nTargets, pCorrect, data=pc.e, color=factor(events),
          facets=visibilityCondition ~ subject) + geom_line()
    qplot(trial.extra.nTargets, pCorrect, data=pc.a, color=factor(appearances),
          facets=visibilityCondition ~ subject) + geom_line()
    qplot(trial.extra.nTargets, pCorrect, data=pc.d, color=factor(disappearances),
          facets=visibilityCondition ~ subject) + geom_line()

    ##this is very noisy data. interestingly enough, appearances and
    ##disappearances seem to hurt things for DT, but not for GB, and
    ##having BOTH hurts things for everyone. Let's hope we can proceed
    ##to demonstrate this more rogorously, as it's a nice story. Let's
    ##look in terms of number visible, perhaps that will tell us something better.

    trials <- transform(trials, which.events = factor(paste(appearances, disappearances)))

    by.visibility <-
        ddply(subset(trials, considered.trials & visibilityCondition != 'full'),
              c("subject", "visibilityCondition", "min.visible", "which.events"),
              summarize, pCorrect = mean(correct), n = length(correct))

    (qplot(min.visible, pCorrect, data=by.visibility,
          color=factor(which.events), facets=subject ~ visibilityCondition, geom="line")
     + geom_point(aes(size=n))
     + scale_color_hue(name="appearances, disappearances"))

    ## that at least makes it a little clearer that "events" really
    ## hurt DT and less so GB. Additionally, appearances seem to hurt
    ## more than disappearances (huh!)

    ##}}}

    ##{{{ Initial real-quick modeling and deciding which "element count" to use.

    ##the strategy is to regress "number of targets" against something
    ##that means the same thing in terms of target density.

    trials$density.equiv <- with(trials, trial.extra.nTargets * ( 1 - arcAngle/2/pi ))

    #the "density equivalent" is slightly different when looking at
    #maxmimum or minimum numbers of targets
    
    trials$max.density.equiv <- with(trials, pmin(ceiling(density.equiv), density.equiv + abs(phaseStep*(trial.motion.process.n-1)/phaseSpacing)))
    trials$min.density.equiv <- with(trials, pmax(floor(density.equiv),   density.equiv - abs(phaseStep*(trial.motion.process.n-1)/phaseSpacing)))

    ##confirm the above meets specs
    ##confirm <- ddply(trials[occluded.trials,], c("trial.extra.nTargets"), with, c(d=mean(density.equiv), max.m=mean(max.visible), max.d=mean(max.density.equiv), min.m=mean(min.visible), min.d=mean(min.density.equiv), n=length(max.visible)))
  
    count.model.specs <- data.frame(
                 column = c("onset.visible", "offset.visible", "mean.visible", "min.visible", "max.visible"),
                 equiv = c("density.equiv", "density.equiv", "density.equiv","min.density.equiv","max.density.equiv"),
                 name = c("n_{\\mathrm{onset}}", "n_{\\mathrm{offset}}","n_{\\mathrm{mean}}", "n_{\\mathrm{min}}", "n_{\\mathrm{max}}"))

    count.models <- alply(count.model.specs, 1, with, {
      fmla <- eval(parse(text=paste("correct ~ (", equiv, "+", column, ") * subject")))
      model <- glm(fmla, data=trials, family=fam, subset=occluded.trials)
    })

    names(count.models) <- count.model.specs$name

    ## Now "max.visible" seems to be the winner. But this is
    ## confusing. Let's plot a confidence ellipse
    ## relating the two coefficients (for subject DT, then transform
    ## for subject GB?)

    winning.count.model <- count.models[[which.min(sapply(count.models, function(m)extractAIC(m)[[2]]))]]

    ##}}}

    ##{{{ per-subject confidence ellipses on the winning simplistic count model

    ## to plot a "confidence ellipse" separately for each subject, we
    ## need to pull out the coefficients pertinent to each
    ## subject. Easiest way I can think of to do this is to
    ## rejigger the factor contrasts repeatedly for each subgroup.

    iterate.contrast.base <- function(model, columns, label="model", xform=function(x)x, ...) {
      ##juggle around the contrasts of a model to answer various
      ##quesitons focused on which subgroup is the "base"
      column.levels <- lapply(columns, function(x) model$xlevels[[x]])
      names(column.levels) <- columns
      factor.table <- expand.grid(column.levels)
      adply(factor.table, 1, function(row) {
        new.contrasts <- model$contrasts
        for (col in colnames(row)) {
          new.contrasts[[col]] <- contr.treatment(x <- model$xlevels[[col]], base=which(x==row[[col]]))
        }
        row[[label]] <- list(glm( model, contrasts=new.contrasts))
        xform(row, ...)
      })
    }

    ##{{{ first ellipse plot

    subject.fits <- iterate.contrast.base(winning.count.model, "subject", label="model")
    #get some ellipses to make with ggplot...
    library(ellipse)
    ellipse.paths <-
         mdply(subject.fits,
               function(model, ...) {
                 data.frame(..., ellipse(model[[1]], which=c(2,3), level=0.67, npoints=100))
               },
               .expand=FALSE)
    central.points <-
         mdply(subject.fits,
               function(model, ...) {
                 cbind(..., splat(data.frame)(model[[1]]$coef[2:3]))
               },
               .expand=FALSE)
    rm(subject.fits)

    #and plot them, with x and y axes labeled and the origin lying on the main diagonal.
    ellipses.plot <- {
      (ggplot(central.points)
       + aes_string(x = names(central.points)[3], y=names(central.points)[4], color="subject")
       + geom_point()
       + geom_path(data=ellipse.paths)
       + geom_hline(yintercept=0, color="grey30")
       + geom_vline(xintercept=0, color="grey30")
       + geom_abline(intercept=0, slope=-1, color="grey30", linetype="dotted")
       + geom_text(aes(label=subject, hjust=-0.1, vjust=1.1), legend=FALSE)
       + scale_x_continuous(limits=c(-.6,.5))
       + scale_y_continuous(limits=c(-.6,.5))
       + coord_equal()
       + opts(legend.position = "none")
       )}

    print.to.pdf <- function(plot, file, ...) {
      pdf(file=file, ...)
      tryCatch(finally=dev.off(), {
        writeLines(file, prodfile)
        print(plot)
      })
    }

    print.to.pdf(ellipses.plot, "figure_2/basic_ellipses.pdf", width=4, height=4)

    ##}}}

    ##{{{  Now let's try with appearances and disappearances stratified into the model. Let's forget those trials with both.

    density.versus.max.events <- {
      glm(
          correct ~ (max.density.equiv + max.visible)
                    *subject
                    *(factor(appearances)*factor(disappearances)),
          data=trials, family=fam,
          subset=occluded.trials
          )
    }

    fit.groups.events <- iterate.contrast.base(density.versus.max.events, c("subject", "factor(appearances)", "factor(disappearances)"))
         
    ellipse.paths.events <-
         mdply(fit.groups.events,
               function(model, ...) {
                 data.frame(..., ellipse(model[[1]], which=c(2,3), level=0.67, npoints=100))
               },
               .expand=FALSE)
    central.points.events <-
         mdply(fit.groups.events,
               function(model, ...) {
                 cbind(..., splat(data.frame)(model[[1]]$coef[2:3]))
               },
               .expand=FALSE)
    rm(fit.groups.events)

    names(central.points.events)[3:4] <- c("appearances", "disappearances")
    names(ellipse.paths.events)[3:4] <- c("appearances", "disappearances")
    central.points.events <- within(central.points.events, events <- factor(paste(appearances, disappearances), labels=c("no crossings", "appearance", "disappearance", "both")))
    ellipse.paths.events <- within(ellipse.paths.events, events <- factor(paste(appearances, disappearances), labels=c("no crossings", "appearance", "disappearance", "both")))

    ugly.events.graph <- (ggplot(central.points.events)
       + aes_string(x = names(central.points.events)[5], y=names(central.points.events)[6], linetype="subject", color="events")
       + scale_color_hue(name="events") 
       + geom_point()
       + geom_path(data=ellipse.paths.events, aes(linetype=subject, color=events))
       + geom_hline(yintercept=0, color="grey30")
       + geom_vline(xintercept=0, color="grey30")
       + geom_abline(intercept=0, slope=-1, color="grey30",linetype="dotted")
       + coord_equal()
       )

    print.to.pdf(ellipses.plot, "figure_2/ugly_ellipses.pdf", width=4, height=4)

    ##}}}

    ##The really surprising thing about this is that the behavior is
    ##very consistent between subjects! That tells me I'm on the right track with this analysis.
    ##Now I wonder what would be easier to read asa graph, more convincing.

    #now let's make a graph combining the basic ellipses
    #with the ones from just appearances
    ##{{{

    ellipses.plot.divided <- (ggplot(rbind(transform(ellipse.paths, trial.subset="all"),
                   transform(subset(ellipse.paths.events, appearances==0 & disappearances==0),
                             trial.subset="no crossings", events=NULL, appearances=NULL, disappearances=NULL)))
     + aes(max.density.equiv, max.visible, linetype=trial.subset, color=subject)
     + geom_path()
     + geom_hline(yintercept=0, color="grey30")
     + geom_vline(xintercept=0, color="grey30")
     + geom_abline(slope=-1, color="grey30", linetype="dotted")
     + geom_point(data=rbind(transform(central.points, trial.subset="all"),
                   transform(subset(central.points.events, appearances==0 & disappearances==0),
                             trial.subset="no crossings", events=NULL, appearances=NULL, disappearances=NULL)))
     )

    print.to.pdf(ellipses.plot.divided, "figure_2/ellipses_divided.pdf", width=4, height=4)

    ##}}}

    ## and interestingly, when broken down by event count, there is not much
    ## difference between subjects except for intercept.
    ##Let's make the figure with pooled subject data.

    ##{{{ Making the graph by pooling subject data.

    density.versus.max.events.no.subject <- glm(
          correct ~ subject + (max.density.equiv + max.visible)
                    *(factor(appearances)*factor(disappearances)),
          data=trials, family=fam,
          subset=occluded.trials
          )

    fit.groups.no.subject <- iterate.contrast.base(density.versus.max.events.no.subject, c("factor(appearances)", "factor(disappearances)"))
         
    ellipse.paths.no.subject <-
         mdply(fit.groups.no.subject,
               function(model, ...) {
                 data.frame(..., ellipse(model[[1]], which=c(3,4), level=0.67, npoints=100))
               },
               .expand=FALSE)
    central.points.no.subject <-
         mdply(fit.groups.no.subject,
               function(model, ...) {
                 cbind(..., splat(data.frame)(model[[1]]$coef[3:4]))
               },
               .expand=FALSE)
    rm(fit.groups.no.subject)

    names(central.points.no.subject)[2:3] <- c("appearances", "disappearances")
    names(ellipse.paths.no.subject)[2:3] <- c("appearances", "disappearances")
    central.points.no.subject <- within(central.points.no.subject, events <- factor(paste(appearances, disappearances), labels=c("no crossings", "appearance", "disappearance", "both")))
    ellipse.paths.no.subject <- within(ellipse.paths.no.subject, events <- factor(paste(appearances, disappearances), labels=c("no crossings", "appearance", "disappearance", "both")))


    no.subject.graph <- (ggplot(central.points.no.subject)
       + aes_string(x = names(central.points.no.subject)[4], y=names(central.points.no.subject)[5], linetype="events")
       + geom_point()
       + geom_path(data=ellipse.paths.no.subject)
       + geom_hline(yintercept=0, color="grey30")
       + geom_vline(xintercept=0, color="grey30")
       + geom_abline(intercept=0, slope=-1, color="grey30",linetype="dotted")
       + coord_equal()
       )

    print.to.pdf(no.subject.graph, "figure_2/no_subject.pdf")

    ##}}}

    ##}}}

    
    ##{{{ Modeling the effect of appearances and disappearances and density for the occluded trials.

    ## the thing to remember here is that I am treating the effect of
    ## appearances/disappearances as essentially arbitrary: they seem
    ## to hurt DT and not so much hurt GB, for instance. What I really
    ## want to show is that even if you know everything about
    ## appearances and disappearances, you still need to have the density to matter.

    ## So let's really try to model appearances and disappearances as
    ## factors in this formula.
    trials <- transform(trials,
                        onset.visible = ifelse(is.na(onset.visible),
                                               trial.extra.nTargets,
                                               onset.visible))

    ##we know that appearances count differently from disappearances
    ##and maybe the intersection needs to be counted too. Here's a
    ##regression model. Notice the min.visible term is inverted and
    ##logged to put it on the same footing as the target spacing
    ##scale.
    events.fmla <- (correct ~ (log(1/min.visible) + appearances + disappearances)*subject)

    ## I'm going to do something like a stepwise regression. Rather
    ## than automating it according to the dumb procedure I'm going to
    ## write a narrative about it, since the variables only make sense
    ## in certain combinations.
    trials$n.s <- -log(trials$min.visible)

    ## Here's a baseline formula; we will have separate intercepts per
    ## subject and for left vs. rigt visibility (which necessarily
    ## happens in different sessions.)
    intercepts.fmla <- correct ~ subject*visibilityCondition
    intercepts.model <- glm(base.fmla, fam, trials, subset=occluded.trials)

    ## why am I not running bootstraps here? I'm working in terms of
    ## 'what predicts subjects' 'correct' responses," not 'what
    ## predicts the PSE. One step lower in the conceptual scale but
    ## the stats are nicer.

    ## note that models i'm not going to say much about have been commented out.

    ## Now first try adding the min. number of targets visible in a
    ## trial.
    ## min.visible.model <- update(intercepts.model, . ~ . + min.visible)
    max.visible.model <- update(intercepts.model, . ~ . + max.visible)
    ## onset.visible.model <- update(intercepts.model, . ~ . + onset.visible)
    ## uh. unexpectedly, fitting to "number visible" seems to make a
    ## better fit than fitting to a scaled version of the same. But
    ## let's roll with that.

    ## These two models are nested. How do we want to compare them?
    ## Let's compare a few different methods. There's the basic test
    ## of the log liklihood chi-square test:
    ## pretty.pval(anova(max.visible.model, intercepts.model, test="Chisq")$P[[2]])

    ## It's like to make a statement about whether the change was significant for each group.
    ## pval.nmin <- anova(intercepts.model, min.visible.model, test="Chisq")$P[[2]]
    ## pval.nmax <- anova(intercepts.model, max.visible.model, test="Chisq")$P[[2]]
    ## pval.onset <- anova(intercepts.model, onset.visible.model, test="Chisq")$P[[2]]

    ## blocking by subject did not improve things at all,
    ## nor did blocking by side of the screen, or both.
    ## max.visible.blocked.subject <- update(max.visible.model, .~.+max.visible:subject)
    ## max.visible.blocked.side <- update(max.visible.model, .~.+max.visible:visibilityCondition)
    ## max.visible.blocked.both <- update(max.visible.model, .~.+max.visible:visibilityCondition*subject)

    ##Note that max.visible + appearances*disappearences is really the
    ##same model space as min.visible + appearances*disappearances or
    ##initial.visible + appearances*disappearances (?)

    ##now we look at appearances and disappearances.
    appearances.model <- update(max.visible.model, .~. + appearances)
    ##yeah, appearances matter, and they impair perormance

    appearances.subject.model <-
       update(max.visible.model, .~. + appearances*subject)
    summary(appearances.subject.model)
    ##yeah, appearances are important for both subjects:
    linearHypothesis(appearances.subject.model, "appearances=0")
    linearHypothesis(appearances.subject.model, "appearances+subjectgb:appearances=0")
    subjects.different = linearHypothesis(appearances.subject.model, "subjectgb:appearances=0")

    ##and there is a signiicant difference between DT and GB in the
    ##effect of appearances, subjectgb:appearances p = 0.03 -- GB
    ##being more weakly affected, but still detrimented
    ##(subjectgb:appearances + appearances)

    appearances.subject.blocked.model <-
       update(max.visible.model, .~. + appearances*subject*visibilityCondition)
    appearances.side.model <-
       update(max.visible.model, .~. + appearances*visibilityCondition)

    ##this barely improves AIC but is momentarily rhetorically useful
    ##(the coefficient of appearances versus the coefficient of adding
    ##another target...)
    appearances.subject.max.model <-
       update(appearances.subject.model, .~. + max.visible:subject)
    appearances.subject.max.sim <- sim(appearances.subject.max.model, 1000)

    ## looking at the ratio of coefficients for "number of targets"
    ## versus "appearances" I wonder if a "common currency" expressing
    ## the effect of appearances in terms of number of targets may be
    ## more parsimonious.
    ## common.currency.appearances.fmla <- (correct ~ subject*visibilityCondition
    ##                                      + max.visible*subject + I(appearances*max.visible))
    ## common.currency.model <- glm(common.currency.appearances.fmla, fam,
    ##                              trials, subset=occluded.trials)
    ## Well, that turns out not to be the right formula for the
    ## model. I'm not sure that this model is cleanly linear... it's
    ## linear with a linear constraint on the coefficients, I suppose.

    ## I have a question about the ratio of appearance factor to
    ## target slope for appearances.subject.model.  Since this is a
    ## question about a ratio, which is a nonlinear function, I need
    ## to run a bootstrap.

    ## Now it would be really useful to me to look at an an
    ##already-fitted model and say, "what would the coefficient of
    ##"appearances" be if "GB, right" were the base case instead of
    ##"DT, left"? this seems like a simple operation that ought to
    ##have an implementation already.  I believe the function
    ##'fit.contrast' is what we need, except it isn't. Instead I can
    ##look at linearHypothesis and express the hypotheses longhand.

    ## anyway, now let's look at the disappearances as an additional
    ## model.

    disappearances.model <- update(appearances.subject.model, . ~ . + disappearances)
    disappearances.subject.model <- update(appearances.subject.model, . ~ . + disappearances*subject)
    disappearances.blocked.model <- update(appearances.subject.model, . ~ . + disappearances*subject*visibilityCondition)
    disappearances.split.model <- update(disappearances.subject.model, . ~ . + max.visible:subject)


    ## something stupid happens when there is both an appearance and a
    ## disappearance. I'd best let those trials go soak.
    crossing.interaction.model <- update(disappearances.model, . ~ . + (appearances*disappearances*subject) + subject:max.visible)
    ## it may be that the "both" trials add enough gunk to the data
    ## that they should just be disregarded. Weird!

    ## disappearances.model looks no worse than
    ## disappearances.subject.model. and the blocked model looks
    ## barely better. in terms of AIC. Let's talk about this more.

    ## Now, appearances and disappearances occur at specified times.
    ## Maybe if we stratify by time we can see something...
    stimulus.length <- with(trials[occluded.trials,], max((trial.motion.process.n - 1) * trial.extra.dt))
    crossing.3breaks <- seq(0, stimulus.length, length.out=3+1)
    crossing.6breaks <- seq(0, stimulus.length, length.out=6+1)
    crossing.9breaks <- seq(0, stimulus.length, length.out=9+1)
    trials <-
       transform(trials,
                 appearance.3bin=cut(appearanceTime, crossing.3breaks, include.lowest=TRUE),
                 disappearance.3bin=cut(disappearanceTime, crossing.3breaks, include.lowest=TRUE),
                 appearance.6bin=cut(appearanceTime, crossing.6breaks, include.lowest=TRUE),
                 disappearance.6bin=cut(disappearanceTime, crossing.6breaks, include.lowest=TRUE),
                 appearance.9bin=cut(appearanceTime, crossing.9breaks, include.lowest=TRUE),
                 disappearance.9bin=cut(disappearanceTime, crossing.9breaks, include.lowest=TRUE)
                 )

    #this only works because I leave appearances as a numeric variable vs. a factor variable.
    both.timing.model <- 
      glm(correct ~ max.visible*subject*visibilityCondition + disappearances + (disappearances:disappearance.6bin)*subject + appearances + (appearances:appearance.6bin)*subject,
          data=trials, subset=occluded.trials & (trials$appearances==0 | trials$disappearances==0), family=fam)

    ## pull out the coefficients for each subject at each time.
    appearance.coefs <-
        iterate.contrast.base(both.timing.model, c("subject", "appearance.6bin"),
                              label="model",xform=within, {
                                coef <- model[[1]]$coef[["appearances"]]
                                coef.num <- model[[1]]$coef[["max.visible"]]
                                coef.sd <- summary(model[[1]])$coef[['appearances', 2]]
                                bin <- appearance.6bin
                                model <- NULL
                              })
    disappearance.coefs <-
        iterate.contrast.base(both.timing.model, c("subject", "disappearance.6bin"),
                              label="model",xform=within, {
                                coef <- model[[1]]$coef[["disappearances"]]
                                coef.num <- model[[1]]$coef[["max.visible"]]
                                coef.sd <- summary(model[[1]])$coef[['disappearances', 2]]
                                bin <- disappearance.6bin
                                model <- NULL
                              })
    appearance.coefs$appearance.6bin <- NULL
    disappearance.coefs$disappearance.6bin <- NULL


    ##what does it look like?
    both.timing.plot <-
      (ggplot(rbind(cbind(appearance.coefs, g="appearances"),
                    cbind(disappearance.coefs, g="disappearances")))
       + aes(x=bin, y=coef, color=subject, group=subject, fill=subject)
       + geom_line()
       + geom_ribbon(aes(ymin=coef-coef.sd,
                         ymax=coef+coef.sd), linetype=0, alpha=0.3)
       + geom_hline(linetype="dotted", yintercept=0)
       + geom_hline(aes(yintercept=coef.num, color=subject, group=subject), linetype="dashed")
       + facet_grid(g ~ .)
       )

    print.to.pdf(both.timing.plot, "figure_2/appearance_timing.pdf", width=4, height=6)

    ##}}}

    ##{{{ now the fancy overlap fogure.

    ## Now throwing target density into the mix.
   trials <- transform(trials, log.eccentricity=log(trial.motion.process.radius))
   ## while we're sorting out the previous section, did you notice that this more or less works?

   #full.model <- glm(correct ~ subject * (max.visible + max.density.equiv) * visibilityCondition * log.eccentricity + (appearances * disappearances * subject), data=trials, subset=occluded.trials, family=fam)
   full.model <- glm(correct ~ subject * (max.visible + max.density.equiv) * visibilityCondition + (appearances * disappearances * subject), data=trials, subset=occluded.trials, family=fam)

   #step according to BIC
   full.model.stepped <- step(full.model, scope=full.model$formula)

   ##clever trick: throw in (density + count) everywhere there is density...
   substitute.formula <- function(fmla, ...) {
     eval(bquote(substitute(.(fmla), list(...))))
   }

   ##make sure that both vars appear everywhere one does
   full.model.stepped.both <-
     update(full.model.stepped,
            formula=substitute.formula(full.model.stepped$formula,
                                       max.density.equiv=quote(max.visible + max.density.equiv),
                                       max.visible=quote(max.visible + max.density.equiv)))
   full.model.stepped.count <-
     update(full.model.stepped.both,
            formula=substitute.formula(full.model.stepped.both$formula,
                                       max.density.equiv=0))
   full.model.stepped.density <-
     update(full.model.stepped.both,
            formula=substitute.formula(full.model.stepped$formula,
                                       max.visible=0))

    ## the x-axis is "target density", the y-axis is "proportion
    ## correct", and we draw a line per "target number." We're going
    ## to interpolate, but only between observed values.
    ## break down using all variables the model uses
    model.columns <- setdiff(colnames(full.model.stepped.both$model), c('correct', 'max.density.equiv'))
    plotpoints <- ddply(trials[occluded.trials,],
                        model.columns,
                        function(group) {
                            data.frame(max.density.equiv = with(group,
                                   seq(min(max.density.equiv)-0.25, max(max.density.equiv)+0.25, by=0.01)))
                          })

   generate.model.predictions <- function(model, points, se.factor=1) {
     link.errors <- predict(model, newdata=points, type="link", se.fit=TRUE)
     cbind(points, with(link.errors,
                        data.frame(correct = model$family$linkinv(fit),
                                   correct.max = model$family$linkinv(fit+se.fit),
                                   correct.min = model$family$linkinv(fit-se.fit))))
   }

   plotpoints.density <- generate.model.predictions(full.model.stepped.density, plotpoints)
   plotpoints.count <- generate.model.predictions(full.model.stepped.count, plotpoints)
   plotpoints.both <- generate.model.predictions(full.model.stepped.both, plotpoints)

    ##and build our figure out of that. Different lines show what the model would be
    ##if it were a vs. b...
    filter <- with(plotpoints,
                   disappearances==0 & appearances==0
                   & !(subject=="dt" & visibilityCondition == "right")
                   & !(subject=="gb" & visibilityCondition == "left")
                   & max.visible <= 5)

    fancy.figure <-
      (ggplot(plotpoints.both[filter,])
       + aes(max.density.equiv, correct, color=factor(max.visible), fill=factor(max.visible))
       + geom_line()
       + geom_ribbon(aes(ymin = correct.min, ymax = correct.max,
                         fill=factor(max.visible),
                         group=factor(paste(appearances, max.visible))), alpha=.3, linetype=0)
       + geom_line(data=plotpoints.density[filter,], linetype=2)
       + geom_line(data=plotpoints.count[filter,], linetype=3)
       + facet_grid(. ~ subject)
       )

    print.to.pdf(fancy.figure, "figure_2/fancyFigure.pdf", width=6, height=3)

    ##oh, also plot residuals so show they aren't far off...

    ##finally, plot ellipses.
    full.model.ellipses <-
      iterate.contrast.base(full.model.stepped.both,
                            c("subject", "visibilityCondition"),
                            label="model",
                            xform = splat(function(model,...) {
                              data.frame(...,
                                         rbind(
                                               cbind(
                                                     rbind(model[[1]]$coef[c("max.visible", "max.density.equiv")]),
                                                     ellipse=FALSE
                                                     ),
                                               cbind(ellipse(model[[1]],
                                                             which=c("max.visible", "max.density.equiv"),
                                                             level=0.67, npoints=100),
                                                     ellipse=TRUE)
                                               ))}))


    ellipse.filter <- with(full.model.ellipses,
                           !(subject=="dt" & visibilityCondition == "right")
                           & !(subject=="gb" & visibilityCondition == "left"))

    final.ellipses.plot <- (ggplot(subset(full.model.ellipses, ellipse.filter & ellipse==FALSE))
     + aes(x = max.density.equiv, y = max.visible, color=subject)
     + geom_point()
     + geom_path()
     + geom_hline(yintercept=0, color="grey30")
     + geom_vline(xintercept=0, color="grey30")
     + geom_text(aes(label=subject, hjust=-0.1, vjust=-0.1), legend=FALSE)
     + coord_equal()
     + geom_path(data = subset(full.model.ellipses, ellipse.filter & ellipse==TRUE))
     + opts(legend.position = "none")
     + scale_x_continuous(limits=c(-.6,.5))
     + scale_y_continuous(limits=c(-.6,.5))
     )

    print.to.pdf(final.ellipses.plot, "figure_2/final_ellipses.pdf", width=4, height=4)

    ##}}}

    ##}}}
    
    ##finally, save the workspace for us to come back to later.
    savefile <- sub('\\..*?$', ".calcs.Rdata", my.args[[2]])
    save(list=ls(), file=savefile)
    writeLines(savefile, prodfile)
  })
}

######

if ("--slave" %in% commandArgs()) { #Rscript...
  my.args <- commandArgs(trailingOnly=TRUE)
  do.call(occlusion.analysis, as.list(script.args))

}
