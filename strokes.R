#!usr/bin/env Rscript

suppressPackageStartupMessages({
  require(ggplot2)
  require(plyr)
  require(binom)
  source("programming.R")
})

process <- function(trials, ...) {

  theme_set(theme_bw())
  theme_update(panel.grid.major = theme_blank(),
               panel.grid.minor = theme_blank())

  ##bin trials over: subject, radius, contrast, displacement, spacing
  data.with.fits <- function(model) {
    pred <- predict(model, type="response", se.fit=TRUE)
    with(pred, data.frame(model$data, fit, se.fit))
  }
  
  ##fit logistic curves to all functions of displacement.
  mkfits <- function(df)
    ddply(  df, c("subject", "contrast", "radius", "spacing", "strokes")
          , mkpipe(  glm(data = ., cbind(p,q) ~ dx, family=binomial(link=logit))
                   , data.with.fits))

  mkpipe(  subset(responseInWindow, select=cols)
         , structure(names=names(cols))
         , ddply(., names(.)[-1]
                  , summarize
                  , p = sum(response == 1), q = sum(response == -1))
         , mutate(  P = p/(p+q)
                  , n = p+q
                  , nTargets = round(radius * 2 * pi / spacing)
                  ##trial.motion.process.n counts from zero (oops)
                  , strokes = strokes + 1)
         , mkfits
         ) -> ratemaker

  cols <- c(  response="folded.response", dx="folded.displacement"
            , subject="subject", contrast="folded.localDirectionContrast"
            , radius="trial.motion.process.radius" , spacing="target.spacing"
            , strokes="trial.motion.process.n"
            )
  rates <- ratemaker(trials)
  
  (ggplot(rates)
   + aes(dx, P, ymin = fit-se.fit, ymax = fit+se.fit
         , color=factor(strokes)
         , fill=factor(strokes)
         , group=factor(strokes))
   + geom_point(aes(size=n))
   + geom_line(aes(y=fit))
   + scale_area()
   + geom_ribbon(color = 0, alpha=0.25)
   + facet_grid(subject ~ spacing)
   + geom_hline(y = 0.5, alpha=0.3)
   + geom_vline(x = 0, alpha=0.3)
   + scale_y_continuous("Proportion of response agreeing with local motion")
   + scale_x_continuous("Displacement per stroke (degrees, + with local motion)")
   + opts(title="Displacement response functions for different motion durations (#strokes.)\nLeft, narrow spacing. Right, wide spacing.")
   ) -> strokes.plot

  print(strokes.plot)
  
}
