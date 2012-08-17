suppressPackageStartupMessages({
	library(ggplot2)
        library(plyr)
        library(ptools)
        library(stringr)
        library(psyphy)
        source("db_functions.R")
        source("graphics_functions.R")
        source("data_functions.R")
        source("good_subjects.R")
})

`%-%` <- setdiff
`%plus%` <- get("%+%", envir=as.environment("package:ggplot2"))

colscale <- function(...) {
  list( scale_color_discrete(...) )
  #list( scale_color_gradientn(..., colours=c(low="#0088FF", mid="#444444", high="#FFAA00"))
  #     , scale_fill_gradientn(..., colours=c(low="#0088FF", mid="#444444", high="#FFAA00")) )
}

main <- function(flist, dbfile, outfile) {
  files <- str_trim(readLines(flist))

  trials <- pull.from.sqlite(dbfile, data.frame(loaded.from=files))

  fout = file(outfile, 'w')
  on.exit(close(fout), add=TRUE)

  ##let's just plot things to verify our calculations
  #diagnostic_pdf_file <- replace_extension(outfile, diag, 'pdf')
  #pdf(diagnostic_pdf_file, onefile=TRUE)
  quartz()
  #writeLines(diag_pdf_file, fout)
  quartzwindow <- dev.cur()
  on.exit(dev.off(quartzwindow), add=TRUE)
  
  threshes <- measure_thresholds(trials, per_session=FALSE,
                                 average_bias=TRUE, sims=500, plot=TRUE)


  
  pdf_file <- replace_extension(outfile, "pdf")
  writeLines(pdf_file, fout)
  pdf(pdf_file, onefile=TRUE)
  on.exit(dev.off(), add=TRUE)
  
  make_figure(threshes)

  data_file <- replace_extension(outfile, "RData")
  writeLines(data_file, fout)
  save(threshes, file=data_file)

  renamed <- rename(threshes, renaming)
  renamed[colnames(renamed) == "drop"] <- list()

  csv_file <- replace_extension(outfile, "csv")
  writeLines(csv_file, fout)
  write.csv(renamed, csv_file, row.names=FALSE)
}

make_figure <- function(threshes) {

  caption1 <- str_c("Bias as a function of directional content. Bias " ,
                    "is as defined in the previous figure; bias can be " ,
                    "interpreted as the frequency with which the observer ",
                    "answers \"clockwise\" to a stimulus with no envelope motion. ",
                    "Colors code different values of element spacing.")
  
  print(plota <- (plot.form(data=threshes, ylim=c(-5, 10))))

  print(plotb <- (
          plot.form(  data=threshes, yvar = slope, ymax = slope + slope.sd
                    , ymin = slope-slope.sd, ylim = c(-5, 50))
          + scale_y_continuous(  "Sensitivity"
                               , breaks=seq(-0,40, by=10), limits=c(-10, 50))
          ))

  caption2 <- str_c( "Sensitivity as a function of directional content. "
                    , "Sensitivity is as defined previously. Higher sensitivity "
                    , "values indicate the observers' responses having a more sensitive "
                    , "dependence on envelope motion. "
                    , "Colors code different values of element spacing.")
  
  grid.newpage()
  grid.draw(with.caption(plota %plus% subset(threshes, subject %in% good_subjects), caption1))

  grid.newpage()
  grid.draw(with.caption(plotb %plus% subset(threshes, subject %in% good_subjects), caption2))
}

qlist <- function(...) {
   x <- match.call(expand.dots="FALSE")[["..."]]
   y <- as.list(substitute(list(...)))[-1]
   y
 }

plot.form <- function(data, ... ,
                      subst.defaults=qlist(
                        , color=factor(target_spacing)
                        , group=factor(target_spacing)
                        , yvar=yint
                        , xvar=folded_localDirectionContrast
                        , ymin=yint-yint.sd, ymax = yint + yint.sd
                        , ylim = c(-3, 5)
                        , xlim = c(-0.1,1.1)
                        )
                      ) {
  
  expr <- quote(  ggplot(data)
                + aes( x=xvar, y=yvar, ymin=ymin, ymax=ymax, color=color, group=group)
                + geom_hline(y=0, alpha=0.5)
                + geom_line()
                + geom_errorbar()
                + theme_bw()
                + scale_x_continuous("Directional content"
                                     , breaks=c(0,  .2, .4, .6, .8, 1)
                                     , labels=c(0,  "", "", "", "", 1))
                + scale_y_continuous("Bias (log odds)", breaks=seq(-5,10,by=5))
                + colscale("Target spacing\n(degrees)"
                           , breaks=unique(data$target_spacing)
                           , labels=format(unique(data$target_spacing), digits=2))
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
