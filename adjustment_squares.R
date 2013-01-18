suppressPackageStartupMessages({
  library(ggplot2)
  library(plyr)
  library(stringr)
  library(ptools)
})

`%--%` <- setdiff
`%++%` <- union

nunion <- function(...)
  Reduce(union, list(...))

automatch <- function(x, y, x.name, y.name) {
  if (y.name == make.names(y.name)) {
    if (paste(y.name, ".i", sep="") %in% colnames(x) && "i" %in% colnames(y)) {
      return(c(paste(y.name, ".i", sep=""), "i"))
    }
  }
  if (y.name == make.names(y.name)) {
    if (paste(x.name, ".i", sep="") %in% colnames(y) && "i" %in% colnames(x)) {
      return(c(paste(x.name, ".i", sep=""), "i"))
    }
  }
  stop("can't hazard a guess on the join keys")
}

pull.down <- function(x , y
                      , .x = if (any(str_length(names(cols.x)))) rename(x, cols.x) else x
                      , .y = if (any(str_length(names(cols.y)))) rename(y, cols.y) else y
                      , by=automatch(.x, .y
                          , deparse(substitute(x),nlines=1)
                          , deparse(substitute(y),nlines=1))
                      , by.x = by[[1]]
                      , by.y = by[[2]]
                      , cols.x=colnames(.x)
                      , cols.y=colnames(.y)
                      , ...
                      ) {
  ###just a version of merge with more useful defaults (and supporting
  ###subsetting/renaming, and explicitly disallowing merge() style
  ###renaming)
  if (length(intersect(union(by.x, cols.x), union(by.y, cols.y))) > 0) {
    stop("Overlapping columns specified.")
  }
  merge(  .x[,union(cols.x, by.x), drop=FALSE]
        , .y[,union(cols.y, by.y), drop=FALSE]
        , by.x=by.x, by.y=by.y, ...)
}

process <- function(showings, endings, trials, runs, parameters, adjusting, ...) {

  adjustments.used <- chain(adjusting
                            , pull.down(runs, cols.y = c(), cols.x="adjusting")
                            , .$adjusting, unique, as.character
                            )

  parameters.used <- chain(parameters
                           , pull.down(runs, cols.y = c(), cols.x="parameters")
                           , .$parameters, unique, as.character
                           )

  ##guess which axes to plot
  if (length(adjustments.used) == 1) {
    ##steal something from parameters used, preferentially.
    aparam <- c(  "trial.extra.globalVScalar"
                 , "trial.extra.nTargets"
                 , "trial.extra.directionContrast") %++% parameters.used %--% adjustments.used
    paramwhich <- which(aparam %in% parameters.used)
    if (length(paramwhich) > 0) {
      adjustments.used <- adjustments.used %++% aparam[paramwhich[1]]
      parameters.used <- parameters.used %--% aparam[paramwhich[1]]
    }
  }

  if (length(adjustments.used) >= 3) {
    parameters.used <- parameters.used %++% adjustments.used[c(-1,-2)]
    adjustments.used <- adjustments.used[1:2]
  }

  if (length(adjustments.used) != 2) {
    stop("oop")
  }

  ##What I am going to do is make this plot on the "adj" axes
  if (length(parameters.used) == 1) {
    faceter = facet_wrap(parameters.used)
  } else if (length(parameters.used) == 2) {
    faceter = facet_grid(paste(parameters.used[1], parameters.used[2], sep = " ~ "))
  } else {
    print("can't handle this many variables!!")
    print(parameters.used)
    return()
  }

  endings <- mutate(endings
                    , accepted = ifelse(type=="a", TRUE, ifelse(type=="r", FALSE, NA))
                    )

  grouped <- ddply(endings
                   , c(adjustments.used, parameters.used, "adjusting", "accepted")
                   , summarize
                   , N = length(accepted)
                   )

  translation <- c(  trial.extra.globalVScalar = "Envelope speed (radians/sec)"
                   , trial.extra.nTargets = "No. of targets"
                   , trial.extra.directionContrast = "Direction contrast"
                   , trial.extra.r = "Eccentricity"
                   )

  ## now let's try to average these shits. Average over the adjustment that was taken atmo.
  ## endings <- ddply(endings
  ##                 , c(adjustments.used, parameters.used,

  print(ggplot(grouped)
   + aes_string(x=as.character(adjustments.used[1]), y=as.character(adjustments.used[2]))
   + aes(color=accepted, shape=adjusting, size=N)
   + scale_shape_manual(breaks=adjustments.used, values=c(15, 18),
                        labels=translation[adjustments.used])
   + geom_vline(x=0, color="gray")
   + geom_point()
   + scale_color_manual("accepted", breaks=c(TRUE, FALSE), values = c("black", "red"))
   + faceter
   + scale_size_area(limits=c(0,5), max_size=3, breaks=1:4)
   + scale_x_continuous(translation[as.character(adjustments.used[1])])
   + scale_y_continuous(translation[as.character(adjustments.used[2])])
   )

}
