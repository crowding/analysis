suppressPackageStartupMessages({
  library(ggplot2)
  library(stringr)
  library(arm)
  source("programming.R")
})

labels <- c(  contrast = "Direction content"
            , spacing="Spacing"
            , instruction="Instruction"
            , radius="Eccentricity(deg)"
            , dx = "Displacement (deg)"
            , P = "Proportion of responses with carrier"
            )

colscale <- function(...) {
  list( scale_color_gradientn(..., colours=c(low="#0088FF", mid="#444444", high="#FFAA00"))
       , scale_fill_gradientn(..., colours=c(low="#0088FF", mid="#444444", high="#FFAA00")) )
}

pretty.numeric.factor <- function(f, digits=3, ...) {
  ##... args passed on to format...
  if (is.ordered(f) || is.numeric(f)) {
    a <- ordered(f)
    levels(a) <- format(as.numeric(levels(a)), digits=digits, ...)
  }
  a
}

`%-%` <- setdiff

keep.if <- function(df,expr) {
  if (eval(substitute(expr), df, parent.frame())) {
    df
  } else {
    df[numeric(0),,drop=FALSE]
  }
}

##bin trials over: subject, radius, contrast, displacement, spacing
data.with.fits <- function(model) {
  pred <- predict(model, type="response", se.fit=TRUE)
  with(pred, data.frame(model$data, fit, se.fit))
}

process <- function(trials, output, ...) {
  
  theme_set(theme_bw())
  theme_update(panel.grid.major = theme_blank(),
               panel.grid.minor = theme_blank())

  if (!"trial.extra.instruction" %in% colnames(trials)) {
    trials$trial.extra.instruction <- factor("none")
  }

  ##there might be more in the future.
  experiment.vars <- c("subject", "contrast", "radius", "spacing", "instruction")
  
  ##fit logistic curves to all functions of displacement. Keep ONE
  ##copy of the fit in a special list-valued column
  mkfits <- function(df, groups=experiment.vars) {
    if (!all(c('p', 'q') %in% colnames(df))) {
         df <- ddply(df, c("dx", groups)
                     , summarize
                     , p = sum(response == 1), q = sum(response == -1))
    }
    ddply(  df
          , groups
          , mkpipe(  model.save <- glm(data = ., cbind(p,q) ~ dx, family=binomial(link=logit))
                   , data.with.fits
                   , mutate(  model=c(list(model.save), vector("list", length(subject)-1))
                            , N = sum(p) + sum(q))
                   , mutate(  P = p/(p+q)
                            , n = p+q
                            , nTargets = round(radius * 2 * pi / spacing)
                            , instruction = ifelse(is.na(instruction), "none", instruction)
                            )
                   ))
  }
  
  mkpipe(subset(responseInWindow, select=cols)
         , structure(names=names(cols))
         , ddply(  ., names(.)[-1]
                 , summarize
                 , p = sum(response == 1), q = sum(response == -1))
         , ddply(names(cols)[c(-1,-2)], function(x) if (nrow(x) > 1) x else NULL)
         , ddply(names(cols)[c(-1,-2)], mkfits)
         , ddply(  experiment.vars %-% "contrast"
                 , mutate
                 , is.contrast.series = length(unique(contrast)) >= 2
                 )
         , ddply(  experiment.vars %-% "spacing"
                 , mutate
                 , is.spacing.series = length(unique(spacing)) >= 2
                 )
         , ddply(  experiment.vars %-% c("spacing", "radius")
                 , mutate
                 , is.radius.series = length(unique(radius)) >= 2
                 )
         ##make this mutate idempotent!
         , mutate(  num.spacing = if (is.numeric(spacing)) spacing else num.spacing 
                  , num.contrast = if (is.numeric(contrast)) contrast else num.contrast
                  , spacing = pretty.numeric.factor(num.spacing)
                  , contrast = pretty.numeric.factor(num.contrast)
                  )
         ) -> ratemaker

  ## cols <- c(  response="abs.response", dx="abs.displacement"
  ##           , subject="subject", contrast="abs.localDirectionContrast"
  ##           , radius="trial.motion.process.radius" , spacing="target.spacing"
  ##           , instruction = "trial.extra.instruction"
  ##           )
  ## ratemaker(trials) -> abs.rates

  cols <- c(  response="folded.response", dx="folded.displacement"
            , subject="subject", contrast="folded.localDirectionContrast"
            , radius="trial.motion.process.radius" , spacing="target.spacing"
            , instruction = "trial.extra.instruction"
            )
  ratemaker(trials) -> folded.rates

  ##This one is kind of nuts.
  (ggplot(folded.rates)
   + aes(dx, P, ymin = fit-se.fit, ymax = fit+se.fit, color=factor(nTargets),
         fill=factor(nTargets), group=factor(nTargets))
   + geom_point(aes(size=p+q))
   + geom_line(aes(y = fit))
   + geom_ribbon(alpha=0.25, color=0)
   + scale_area()
   ) -> wrap.plot
  print(wrap.plot + facet_wrap(~subject+contrast+radius))
  #that'll do for now. let me get my makefile running.

  ##Now what I want to see is a square for every series.
  ##Series is defined as: at same subject and eccentricity,
  ##three or more spacings at the same contrast.
  ##Or two or more contrasts at same spacing.

  series.plot <- function(df, series.var, facet.var) {
    ex <- substitute(series.var)
    facex <- substitute(facet.var)

    title = if (length(unique(df[[deparse(facex)]])) > 1) {
      sprintf(  "Subject %s. Colors denotes %s, subplots ordered by %s."
              , toupper(unique(df$subject)), tolower(labels[[deparse(ex)]])
              , tolower(labels[[deparse(facex)]]) )
    } else {
       sprintf(  "Subject %s. Colors denote %s, with constant %s %s."
               , toupper(unique(df$subject)), deparse(ex)
               , tolower(labels[[deparse(facex)]]), tolower(unique(df[[deparse(facex)]])))
    }

    eval(substitute(
           (ggplot(df)
            + aes(dx, P, ymin = fit-se.fit, ymax = fit+se.fit
                  , color=as.numeric(ex)
                  , fill=as.numeric(ex)
                  , group=ex)
            + geom_point(aes(size=n))
            + scale_area()
            + geom_line(aes(y = fit))
            + geom_ribbon(alpha=0.25, color=0)
            + geom_vline(x = 0, alpha=0.3)
            + geom_hline(y = 0.5, alpha=0.3)
            + colscale(  name=labels[[deparse(quote(ex))]]
                       , breaks=1:length(levels(df[[deparse(quote(ex))]]))
                       , labels=levels(df[[deparse(quote(ex))]]))
            + scale_y_continuous("Responses in direction of carrier", breaks=c(0,0.5,1))
            + scale_x_continuous("Displacement (in direction of carrier)")
            + facet_grid(facex ~ subject, labeller=function(y,x)format(x, digits=3))
            + opts(title=title)
            )
           , list(ex=ex, facex=facex)))
  }

  ##Gimme a page for contrast and segment series for each subject.
  d_ply(subset(folded.rates, abs(radius - 6.67) < 0.1)
        , "subject"
        , function(df) {
          if (any(df$is.contrast.series)) {
            pipe(df
                 , subset(is.contrast.series)
                 , series.plot(., contrast, spacing)
                 , print
                 )
          }
          if (any(df$is.spacing.series)) {
            pipe(df
                 , subset(is.spacing.series)
                 , series.plot(., spacing, contrast)
                 , print
                 )
          }
        })

  ##Let's now look at the effect of instruction.
  ##Get me all the subsets where instruction is varied.
  ##Look at a comparison between instructed and non-instructed.
  d_ply(  ddply(  folded.rates
                , experiment.vars %-% c("instruction", "spacing")
                , keep.if, length(unique(instruction)) > 1
                )
        , experiment.vars %-% c("instruction", "spacing")
        , function(x) print(series.plot(x, spacing, instruction))
        )

  ##Let's look at reproducibility. Get all the trials that tested the
  ##same thing on multiple days, and make a fit for each day. Plot the
  ##x-intercept, y.intercept, slope and etc.
  ##
  ##Since sometimes there are more than one run per day, I need ot do
  ##it by day, so I string munge the source file column.
  ##Thsi will also tell me whether slope or bias or x-intercept is more reliable.
  ##
  ##Fit logits to every function that was duplicated on separate days.
  extract.coefs <- function(x, thresholds=c(0.20,0.50,0.80)) {
    pipe(x
         , subset(!vapply(model, is.null, logical(1)))
         , adply(., 1, function(r) {
           m <- r$model[[1]]
           r[["model"]] <- NULL
           s <- summary(m)
           ##simulate to pull the threshold. quick and dirty.
           ##I know ARM code does an extra correction...
           simul <- mvrnorm(500, s$coefficients[,1,drop=T], (s$cov.scaled))
           thresh <- t(laply(thresholds,function(t) (m$family$linkinv(t) - simul[,1])/simul[,2]))
           iqr <- t(laply(c(0.25,0.75),function(t) (m$family$linkinv(t) - simul[,1])/simul[,2]))
           iqr <- iqr[,2] - iqr[,1]
           colnames(thresh) <- paste("thresh", thresholds*100, sep='.')
           rownames(r) <- NULL
           simul <- cbind(simul, thresh, iqr)
           cbind(r
                 , q10=apply(simul, 2, quantile, .10)
                 , q50=apply(simul, 2, quantile, .5)
                 , q90=apply(simul, 2, quantile, .90)
                 , coef=colnames(simul))
         })
       )
  }

  sortorder <- list("subject", "instruction", "contrast", "radius", "spacing")

  pipe(  trials
       , subset(responseInWindow, select = c(cols, "source.file"))
       , structure(names=c(names(cols), "source.file"))
       , mutate(  source.file=str_extract(source.file, '\\d\\d\\d\\d-\\d\\d-\\d\\d')
                , instruction = ifelse(is.na(instruction), "none", instruction)
                )
       , ddply(c(experiment.vars, "source.file"),
               keep.if, length(source.file) > 50
               )
       , ddply(  experiment.vars
               , keep.if, length(unique(source.file)) > 1
              )
       ) -> replication.trials
  if (nrow(replication.trials) > 0) {
    pipe(replication.trials
       , mkfits( c(experiment.vars, "source.file") )
       , extract.coefs
       , mutate(  .
                , model = NULL
                , case = do.call(paste, c(lapply( as.list(.[experiment.vars])
                                                , format,digits=3)
                    , sep=" | "))
                , case = do.call(paste, c(lapply( as.list(.[c("source.file", experiment.vars)])
                                    , format,digits=3)
                                      , sep=" | "))
                , entry = do.call(paste, c(as.list(.[c("source.file", experiment.vars)]), sep=" | "))
              )
       ) -> repeatability

  pipe(folded.rates
       , extract.coefs
       , mutate(  .
                , case=do.call(paste
                    , c(lapply(as.list(.[experiment.vars])
                               , format, digits=3)
                    , sep=" | ")) )
       , merge(., repeatability, by=c("case", "coef"), suffixes=c(".avg", ".each"))
       ) -> joined.repeatability

  ##exluding merely to set the scale on my graph....
  exclude <- c("jb  | 0.20 |  6.67 |  2.094 | none "
               , "pbm | 0.00 |  6.67 |  8.378 | none "
               , "cf  | 0.00 |  6.67 |  8.378 | none "
               , "ns  | 0.20 |  6.67 |  2.094 | none "
               , "nj  | 0.20 |  2.96 |  0.931 | none ")

  (ggplot(subset(joined.repeatability,
                 (!case %in% exclude) & (coef %in% c("iqr", "thresh.50", "thresh.80"))))
   + scale_y_continuous(expand=c(0,0))
   + aes(x=case
         , y =  q50.each
         , ymin=q10.each
         , ymax=q90.each
         )
   + geom_errorbar(alpha=0.3)
   + facet_grid(coef~., scales="free")
   + geom_errorbar(aes(ymin = q10.avg, ymax = q90.avg), color="red")
   + geom_point(alpha=0.3) #+ geom_point(aes(size=N.each))
   + opts(axis.text.x=theme_text(angle=90, hjust=1),
          title="Measurements of inverse slope (iqr) and threshold\ntaken over single days (gray) and combined (red)")
   ) -> repeatability
    print(repeatability)
  }
  
  ##Save our objects in case we want to mess with graphs later...

  filename <- replace_extension(summary(output)$description, "RData")
  writeLines(filename, output)
  save(file=filename, list=ls())
}

replace_extension <- function(filename, new_extension) {
  sub(  "((.)\\.[^.]*|)$"
      , paste("\\2.", new_extension, sep="")
      , filename)
}
