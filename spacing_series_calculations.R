suppressPackageStartupMessages({
	library(ggplot2)
        library(plyr)
        library(ptools)
        library(stringr)
        library(psyphy)
        source("db_functions.R")
        source("modeling.manipulations.R")
})

columns.to.pull <- c("trial.motion.process.radius",
                     "trial.extra.side",
                     "trial.extra.nVisibleTargets",
                     "abs.displacement",
                     "abs.localDirectionContrast",
                     "trial.extra.tf",
                     "trial.extra.wavelengthScalar",
                     "trial.extra.dt",
                     "trial.extra.widthScalar",
                     "trial.extra.durationScalar",
                     "trial.extra.nTargets",
                     "trial.motion.process.order",
                     "trial.motion.process.n",
                     "visibilityCondition",
                     "folded.localDirectionContrast",
                     "folded.displacement",
                     "folded.response",
                     "abs.response",
                     "target.spacing",
                     "responseInWindow",
                     "responseTime",
                     "maxResponseTime",
                     "loaded.from",
                     "runs.i",
                     "subject"
                     )

#main(flist = "collections/spacing_series.list", dbfile = "discrimination.sqlite", outfile = "collections/spacing_series_calculations.out")
#trials <- pull.from.db(conn, data.frame(loaded.from=str_trim(readLines("collections/spacing_series.list"))))
main <- function(flist, dbfile, outfile) {
  
  drv <- SQLite()
  files <- str_trim(readLines(flist))
  trials <- with.db.connection(drv, dbfile, fn=function(conn) {
    pull.from.db(conn, data.frame(loaded.from=files))
  })

  threshes <- measure_thresholds(trials, per_session=FALSE)
  session <- measure_thresholds(trials, per_session=TRUE)

  ##we'll put out an Rdata file and a .pdf file and a .csv file for Ione.
  fout <- file(outfile, 'w')
  on.exit(close(fout), add=TRUE)

  csv_file <- replace_extension(outfile, "csv")
  writeLines(csv_file, fout)
  write.csv(threshes, csv_file)
  
  pdf_file <- replace_extension(outfile, "pdf")
  writeLines(pdf_file, fout)
  make_figure(threshes, pdf_file)

  data_file <- replace_extension(outfile, "RData")
  writeLines(data_file, fout)
  save(threshes, session, file=data_file)
}

replace_extension <- function(filename, new_extension) {
  sub(  "((.)\\.[^.]*|)$"
      , paste("\\2.", new_extension, sep="")
      , filename)
}

measure_thresholds <- function(trials,per_session=FALSE) {

  split <- c(  "subject"
             , "target_spacing"
             , "trial_motion_process_radius"
             , "folded_localDirectionContrast"
             )
  if (per_session) split <- union(split, "loaded_from")
  used <- c("folded_displacement", "folded_response", "responseInWindow")
  ddply(trials[union(split, used)], split, psychometric_function, .progress="text")
}

psychometric_function <- function(data) {
  #return slope, threshold, and quantiles of each from simulation
  data <- mutate(data, response.cw = folded_response > 0)
  fit <- glm(  response.cw ~ folded_displacement
             , binomial(link=logit.2asym(0.05, 0.05))
             , subset(data, as.logical(responseInWindow))
             )

  ##note this will find a bunch of intercepts (arg 2) if you want...
  cases <- data[1,,drop=FALSE]
  X <- find.intercept.glm(  fit, cases
                                   , 'folded_displacement'
                                   , response = c(.5, .75)
                                   , result.type="list"
                                   , sims=500
                                   )

  with(  X
       , c(  bias = intercept[[1]]
           , c(bias = quantile(sim[[1]], c(0.1,0.25,0.50,0.75,0.9)))
           , threshold = intercept[[2]] - intercept[[1]]
           , c(threshold = quantile(sim[[2]] - sim[[1]], c(0.1,0.25,0.50,0.75,0.9)))
           , yint = fit$coefficients[[1]]
           , yint.sd = sqrt(vcov(fit)[[1,1]])
           , slope = fit$coefficients[[2]]
           , slope.sd = sqrt(vcov(fit)[[2,2]])
           , slope.positive <- mean(sign(X$sim[[2]]))
           )
       )

}


#make a "list of quoted items"
qlist <- function(...) {
   x <- match.call(expand.dots="FALSE")[["..."]]
   y <- as.list(substitute(list(...)))[-1]
   y
 }

plot.form <- function(  dataset
                      , subst.defaults=qlist(
                            color=factor(folded_localDirectionContrast)
                          , group=factor(folded_localDirectionContrast)
                          , yvar = yint
                          , ymin = yint+yint.sd , ymax = yint-yint.sd
                          , ylim=c(-5, 15) , xlim=c(0,20)
                          , xlabel=
                          )
                      , ...
                      ) {
  
  expr <- quote(ggplot(dataset)
                + theme_bw()
                + aes(  x = target_spacing , y = yvar
                      , ymin = ymin , ymax = ymax
                      , color = color , group = group
                      )
                + geom_errorbar()
                + geom_line()
                + coord_cartesian(ylim=ylim, xlim=xlim)
                + scale_x_continuous("Target spacing (degrees)", breaks=seq(0,20,by=5))
                + scale_color_discrete("Directional\ncontent")
                + facet_wrap( ~ subject)
                )
  
  s <- subst.defaults
  subst <- as.list(substitute(list(...)))[-1]
  s[names(subst)] <- subst
  eval(substitute.nq(expr, s))
}

make_figure <- function(threshes, pdf_file) {
  pdf(pdf_file)
  on.exit(dev.off(), add=TRUE)

  plot1 <- ( plot.form(threshes) +
             scale_y_continuous("Bias (log odds)", breaks=seq(0,20,by=5)))
  
  with.caption( plot1 ,
                  "Bias as a function of target spacing and carrier motion direction. The bias is defined as the y-intercept of the psychometric function, that is, the rate at which the subject would answer \"clockwise\" for a simulus that has no no motion. The bias is plotted in log-odds on the vertical axis. Directional content describes the mixture of CW and CCW carrier components; a value of 1 only has a counterclockwise carrier, while a value of 0 has both carriers in counterphase. Clockwise (positive) and counterclockwise (negative) direction content were tested, but both types of trials are folded into clockwise for this graph. Error bars inticate +- standard error of the estimate."
               ) -> fr
  
  grid.draw(fr)

  plot2 <- (plot.form(threshes, yvar=slope, ymin=slope-slope.sd, ymax=slope+slope.sd, ylim=c(-5,50))
   + scale_y_continuous("Sensitivity", breaks=seq(-0,40, by=10))
   )

  with.caption(plot2 ,
               "Sensitivity to envelope motion, as a function of target spacing and directional content. The sensitivity is defined as the slope parameter of a logistic function over the envelope motion. For example a sensitivity of 10 means that changing envelope motion by 1 degree/sec will change the response rates from 50% clockwise to 73% clockwise.") -> fr2

  grid.newpage()
  grid.draw(fr2)

  #We can also do fitting per session.
}
  
with.caption <- function(plot, caption, gp=gpar(fontsize=8)) {
  wrap <- paste(strwrap(caption, width=80), collapse="\n")
  text <- textGrob(wrap, just="left", x=0)
  fr <- frameGrob(name="fr")
  fr <- packGrob(fr, text, side="bottom")
  fr <- packGrob(fr, ggplotGrob(plot), side="top")
  fr
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("main", as.list(args))
}
