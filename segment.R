#!usr/bin/env Rscript

suppressPackageStartupMessages({
  require(ggplot2)
  require(plyr)
  require(binom)
  source("programming.R")
})

process <- function(trials, output, ...) {
  theme_set(theme_bw())
  theme_update(panel.grid.major = theme_blank(),
               panel.grid.minor = theme_blank())

  relevant.cols <- c( response="folded.response", side="trial.extra.side"
                     , dx="folded.displacement", "subject"
                     , contrast="folded.localDirectionContrast"
                     , number="trial.extra.nVisibleTargets", spacing="target.spacing"
                     )

  chain(trials
       , subset(responseInWindow, select=relevant.cols)
       , ddply( relevant.cols[-1]
               , summarize
               , p = sum(folded.response==1)
               , q = sum(folded.response==-1))
        , transform(  conf = binom.confint(p, p+q, method="logit", conf.level=0.75)
                    , P = p/(p+q)
                    , n = p+q
                )
       ) -> rates

  chain(trials
       , subset(responseInWindow, select=relevant.cols)
       , ddply( relevant.cols[-1:-2]
               , summarize
               , p = sum(folded.response==1)
               , q = sum(folded.response==-1)
               )
       , transform( conf = binom.confint(p, p+q, method="logit", conf.level=0.75)
                   , P = p/(p+q)
                   , n = p+q
                   , side="all"
                   )
       ) -> folded.rates

  chain(trials
       , subset(responseInWindow & folded.displacement < 0.4 & folded.localDirectionContrast != 0, select=relevant.cols)
       , ddply( relevant.cols[-1:-3]
               , summarize
               , p = sum(folded.response==1)
               , q = sum(folded.response==-1))
       , transform( conf = binom.confint(p, p+q, method="logit", conf.level=0.75)
                   , P = p/(p+q)
                   , n = p+q
                   , side="all"
                   , dx="all"
                   )
       ) -> dx.folded.rates
  

  for (dataset in list(dx.folded.rates, folded.rates, rates)) {
    graph <- (ggplot(dataset)
              + aes(spacing, p/n, ymin = conf.lower, ymax = conf.upper)
              + geom_line() + facet_wrap(~subject+side+dx+contrast) + geom_errorbar()
              + geom_point())
    
    print(graph + aes(x = spacing, color=factor(number)))
    print(graph + aes(x = number, color=factor(spacing)))
  }

  ## now what about subjects where I tested more than one
  ## displacement? Draw rates as a function of displacements...
  ## chain( folded.rates
  ##      , subset(n > 100)
  ##      , ddply(  c("subject", "side", "contrast")
  ##              , function(x) if (length(unique(x$dx)) > 2) x else NULL)
  ##     # , pad.missing.cells(c("subject", "side", "contrast"))
  ##      , (ggplot(.)
  ##         + aes(dx, p/n, color=spacing, group=interaction(number,spacing))
  ##         + geom_point()
  ##         + geom_line()
  ##         + facet_wrap(~contrast+subject+side)
  ##         )
  ##      ) -> dx.plot
  ## print(dx.plot)
  
}
