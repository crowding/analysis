suppressPackageStartupMessages({
	library(ggplot2)
        library(plyr)
        library(ptools)
        library(stringr)
        library(psyphy)
        source("db_functions.R")
        source("graphics_functions.R")
        source("data_functions.R")
})

main <- function(flist, dbfile, outfile) {
  files <- str_trim(readLines(flist))

  trials <- pull.from.sqlite(dbfile, data.frame(loaded.from=files))

  fout = file(outfile, 'w')
  on.exit(close(fout), add=TRUE)

  threshes <- measure_thresholds(trials, per_session=FALSE)

  pdf_file <- replace_extension(outfile, "pdf")
  writeLines(pdf_file, fout)
  pdf(pdf_file)
  on.exit(dev.off(), add=TRUE)
  make_figure(threshes)

  data_file <- replace_extension(outfile, "RData")
  writeLines(data_file, fout)
  save(threshes, session, file=data_file)

  
}

make_figure <- function(threshes) {
  caption <- "
 Bias as a function of directional content. Bias is as defined in the previosu figure. Colors code different values for element spacing.
"

  plot.form()

}

qlist <- function(...) {
   x <- match.call(expand.dots="FALSE")[["..."]]
   y <- as.list(substitute(list(...)))[-1]
   y
 }

plot.form <- function(
                      ... ,
                      subst.defaults=qlist(
                        , data=threshes
                        , color=factor(target_spacing)
                        , group=factor(target_spacing)
                        , yvar=yint
                        , xvar=folded_localDirectionContrast
                        , ymin=yint-yint.sd, ymax = yint + yint.sd
                        , ylim = c(-5, 10)
                        , xlim = c(-0.1,1.1)
                        , )
                      ) {
  
  expr <- quote(  ggplot(data)
                + aes( x=xvar, y=yvar, ymin=ymin, ymax=ymax, color=color, group=group)
                + geom_line()
                + geom_errorbar()
                + theme_bw()
                + scale_x_continuous("Directional content"
                                     , breaks=c(0, .1, .2, .5, 1))
                + scale_y_continuous("Bias (log odds)", breaks=seq(-5,10,by=5))
                + scale_color_discrete("Target spacing (degrees)")
                + facet_wrap( ~ subject)
                + coord_cartesian(ylim=ylim, xlim=xlim)
                )
  
  s <- subst.defaults
  subst <- as.list(substitute(list(...)))[-1]
  s[names(subst)] <- subst
  eval(substitute.nq(expr, s))
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("main", as.list(args))
}
