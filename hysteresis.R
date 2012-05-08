suppressPackageStartupMessages({
  library(ggplot2)
  library(plyr)
  library(stringr)
  library(ptools)
})

process <- function(showings, endings, trials, runs, ...) {
  ## I imagine a raster-style plot.
  ##
  ##Down the vertical axis, all individual trials, sorted by the
  ##combination of conditions used.
  ##
  ##Down the horizontal axis, all the local trials, sorted by the combination of conditions used.
  ##
  ##Across the time axis, aligned to the last presentation which was "accepted"
  ##A symbol plotted for each adjustment up, and a symbol for each adjustment down.
  ##A symbol for the start of each presentation, which scales according to the adjustment.
  ##Symbols or each step up and each step down.

  theme_set(theme_bw())
  theme_update(panel.grid.major = theme_blank(),
               panel.grid.minor = theme_blank())

  ## x <- rbind.fill(plot.trials, showings, endings, turns)

  ## (ggplot(turns) + aes(alignedTime, i, color=type) + geom_point(size=0.5))

  ## (ggplot(showings)
  ##  + aes(x = alignedTime, y = i, xend=alignedTime+duration, yend=i, color=rel.value)
  ##  + geom_segment(size=1)
  ##  + scale_colour_gradient2(low="cyan", high="yellow", mid="black")
  ##  )

  ## (ggplot(endings)
  ##  + aes(x = alignedTime, y = i, color=type)
  ##  + geom_point())

  ##visual check for hysteresis: color coding by final value, show the history that led there.
  print(ggplot(merge(  rbind.fill(  showings
                             , mutate(showings, alignedTime=alignedTime+duration)
                             , endings)
                , endings[,c("trials.i","value", "rel.value")]
                , by="trials.i", suffixes=c(".during", ".after")
                )
          )
   + aes(x = alignedTime, y = value.during, color=rel.value.after, group=trials.i)
   + geom_line(size=1, alpha=.2)
   + facet_grid(adjusting ~ ., scales="free")
   + scale_colour_gradient2(low="red", high="cyan", mid="black")
   )

  ##simpler check for hysterisis
  print(ggplot(merge(ddply(showings, "trials.i",
                      function(x) x[which.min(x$presentationTime),])
                , endings
                , by="trials.i", suffixes = c(".start", ".end")))
   + aes(x = value.start, y = value.end)
   + facet_wrap(~ adjusting.start, scales="free")
   + opts(aspect_ratio=1)
   + geom_abline(color="gray", slope=1, intercept=0)
   + coord_equal()
   + geom_point()
   + geom_jitter()
   )
  
 ##Hokay, let's select the ones where the sign flips or doesn't.
                                                              
 ##Something is wrong with the globalVScalar adjustment??? Half of
 ##them flip sign in the last adjustment. Hmm. It's because of the
 ##way I back out the extra-things
}
