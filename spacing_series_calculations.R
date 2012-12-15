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
        source("helper_functions.R")
        source("programming.R")
})

keep.if <- function(df,expr) {
  if (eval(substitute(expr), df, parent.frame())) {
    df
  } else {
    df[numeric(0),,drop=FALSE]
  }
}

main <- function(flist, dbfile, outfile) {

  files <- str_trim(readLines(flist))
  trials <- pull.from.sqlite(dbfile, data.frame(loaded.from=files))

  ##we'll put out an Rdata file and a .pdf file and a .csv file for Ione.
  fout <- file(outfile, 'w')
  on.exit(close(fout), add=TRUE)
  
  ##let's just plot things to verify our calculations
#  diagnostic_pdf_file <- replace_extension(outfile, 'pdf', '_diagnostic')
#  pdf(diagnostic_pdf_file, onefile=TRUE)
#  diag <- dev.cur()
#  on.exit(dev.off(diag), add=TRUE)
#  writeLines(diagnostic_pdf_file, outfile)
#  if (!names(dev.cur()) %in% c("quartz", "X11", "windows")) {
#    quartz()
#    quartzwindow <- dev.cur()
#    ##on.exit(dev.off(quartzwindow), add=TRUE)
#  }
  
  threshes <- measure_thresholds(trials, per_session=FALSE,  sims=500, plot=FALSE)
  pdf_file <- replace_extension(outfile, "pdf")
  writeLines(pdf_file, fout)
  make_figure(threshes, pdf_file)

  data_file <- replace_extension(outfile, "RData")
  writeLines(data_file, fout)
  save(threshes, file=data_file)

  renamed <- rename(threshes, renaming)
  renamed[colnames(renamed) == "drop"] <- list()

  csv_file <- replace_extension(outfile, "csv")
  writeLines(csv_file, fout)
  write.csv(renamed, csv_file, row.names=FALSE)
}


#make a "list of quoted items"
qlist <- function(...) {
   x <- match.call(expand.dots="FALSE")[["..."]]
   y <- as.list(substitute(list(...)))[-1]
   y
 }

plot.form <- function(  dataset
                      , subst.defaults=qlist(
                            group=factor(folded_localDirectionContrast)
                          , yvar = yint
                          , ymin = yint+yint.sd , ymax = yint-yint.sd
                          , ylim=c(-5, 10) , xlim=c(0,20)
                          )
                      , ...
                      ) {
  
  expr <- quote(
                ggplot(dataset)
                + theme_bw()
                + aes(  x = target_spacing , y = yvar
                      , ymin = ymin , ymax = ymax
                      , group = group
                      )
                + geom_errorbar()
                + geom_line()
                + coord_cartesian(ylim=ylim, xlim=xlim)
                + scale_x_continuous("Target spacing (degrees)", breaks=seq(0,20,by=5))
                + facet_wrap( ~ subject)
                + geom_hline(y=0, alpha=0.3)
                )
  
  s <- subst.defaults
  subst <- as.list(substitute(list(...)))[-1]
  s[names(subst)] <- subst
  eval(substitute.nq(expr, s))
}

make_figure <- function(threshes, pdf_file) {
  pdf(pdf_file, onefile=TRUE)
  on.exit(dev.off(), add=TRUE)

  contrasts <- unique(threshes$contrast)
  
  ddply(threshes
        , "subject"
        , mkchain(  ddply("folded_localDirectionContrast"
                          , mutate, N = sum(n))
                  , subset(., N == max(N))
                  , subset(folded_localDirectionContrast == max(folded_localDirectionContrast))
                  , ddply("subject", keep.if, all(subject %in% good_subjects))
                  )
        ) -> onethresh

  plot1 <- (  plot.form(threshes)
            + aes(color=factor(folded_localDirectionContrast))
            + scale_color_discrete("Directional\ncontent")
            + scale_y_continuous("Bias (log odds)", breaks=seq(0,20,by=5), limits=c(-5,10))
            )

  plot1b <- ( plot.form(onethresh)
             + scale_y_continuous("Bias (log odds)", breaks=seq(0,20,by=5), limits=c(-5,10))
             )

  print(plot1)
  print(plot1b)
  
  with.caption( plot1b ,
               "Bias as a function of target spacing. The bias is defined as the y-intercept of the psychometric function, that is, the rate at which the subject would answer \"clockwise\" for a simulus that has no motion. The bias is plotted in log-odds on the vertical axis. Directional content describes the mixture of CW and CCW carrier components; a value of 1 only has a counterclockwise carrier, while a value of 0 has both carriers in counterphase. Clockwise (positive) and counterclockwise (negative) direction content were tested, but both types of trials are folded into clockwise for this graph. Error bars inticate +- standard error of the estimate."
               ) -> fr

  grid.newpage()
  grid.draw(fr)


  
  plot2 <- (plot.form(threshes, yvar=slope, ymin=slope-slope.sd, ymax=slope+slope.sd, ylim=c(-5,50))
   + scale_y_continuous("Sensitivity", breaks=seq(-0,40, by=10), limits=c(-10, 50))
            + aes(color=factor(folded_localDirectionContrast))
            + scale_color_discrete("Directional\ncontent")
   )

  plot2b <- (plot.form(onethresh, yvar=slope, ymin=slope-slope.sd, ymax=slope+slope.sd, ylim=c(-5,50))
             + scale_y_continuous("Sensitivity", breaks=seq(-0,40, by=10), limits=c(-10, 50))
   )

  print(plot2)
  print(plot2b)
  
  with.caption(plot2b ,
               "Sensitivity to envelope motion, as a function of target spacing. The sensitivity is defined as the slope parameter of a logistic function over the envelope motion. For example a sensitivity of 10 means that changing envelope motion by 1 degree/sec will change the response rates from 50% clockwise to 73% clockwise.") -> fr2

  grid.newpage()
  grid.draw(fr2)
  
  #We can also do fitting per session.
}

run_as_command()
