#merely describe the parameters that were varied in an experiment.

suppressPackageStartupMessages({
  library(plyr)
  library(ptools)
  library(stringr)
})

do.describe <- function(datafile, ...) {
  infiles <- setdiff(c(...), datafile)
  descriptions <- new.env(parent=globalenv())
  if (file.exists(datafile)) load(datafile, envir=descriptions)
  on.exit(save(list = ls(descriptions), file=datafile, envir=descriptions))
  if (!"descriptions" %in% ls(descriptions)) descriptions$descriptions <- list()

  for (i in infiles) {
    cat("describing ", i, "\n")
    description <- describe.file(i)
    descriptions$descriptions[i] <- description
  }
#  capture.output(file=outfile, print(table, width=Inf))
}

describe.file <- function(infile) {
  data.env <- new.env(parent=globalenv())
  load(infile, envir=data.env)
  table <- do.call(describe, as.list(data.env))
  rownames(table) <- NULL
  structure(list(table), names=infile)
}

describe <- function(trials, runs, ...) {
  #try to boil down the experiment into the discrete cases that were tested.
  #This being applied after "common" assume that only real trials are calculated.

  #which variables are on quest/staircase? Replace those with "varied"

  ignored <- c(  "trial.textFeedback.text"
               , "trial.extra.localDirection", "trial.extra.globalDirection"
               , "trial.motion.process.color", "trial.motion.process.velocity"
               , "trial.parameterValues", "trial.extra.flankerAngle"
               , "trial.extra.flankerPhase", "trial.motion.process.phase"
               , "trial.motion.process.dphase", "trial.motion.process.angle"
               , "trial.extra.max_extent", "trial.extra.min_extent", "trial.extra.color"
               , "trial.awaitInput", "trial.desiredResponse", "trial.run.i", "trial.extra.phase"
               )
  valued <- c(  "trial.extra.r", "motionCondition", "trial.extra.visibilityCondition"
              , "n.occluders", "folded.localDirectionContrast")
  
  required <- chain(c(  "subject"
                        , "trial.extra.r"
                        , "folded.localDirectionContrast"
                        , "trial.extra.nTargets")
                    , union(valued)
                    , union(what.varies(runs))
                    , intersect(colnames(trials))
                    , setdiff(ignored)
                    )

  permitted <- chain(  required
                     , union(grep("^trial", colnames(trials), value=TRUE))
                     , intersect(colnames(trials))
                     , setdiff(ignored)
                     )
                  
  ord <- chain(  list(valued, required, grep("^trial.extra", permitted, value=TRUE), permitted)
               , Reduce(union, .)
               )

  staircased <- intersect(what.staircases(runs), colnames(trials))
  
  kept <- union(grep("^trial.extra", permitted, value=TRUE), required)
  
  deflated <- deflate.data.frame(  trials, order=ord, ignored=ignored)

  ##I care about things that vary over a few cases (parameters) but
  ##not things that vary on every trial (random placement, reaction
  ##times... Therefore I should see if there is a way to pull out
  ##which aprameters were set by staircases.
  interesting.columns <-
    chain(  deflated$values
          , sapply(length)
          , .[.>1 & .< 30]
          , names
          , union(required)
          )

  ##I wonder if there is a way to sort out of a column is dependent on
  ##two rather than one previous columns (e.g. velocity, when spatial
  ##frequency and eccentricity are varied)

  ## If an interesting column is dependent on a previous column, and
  ## is not in "kept", it is not interesting (e.g. color)
  interesting.columns <-
    chain(interesting.columns,
          .[deflated$columns[.] == .])

  ## And columns that are staircased get
  staircase <- factor("varied")

  ##Anyway, the description is the sorted list of unique conditions
  ##defined by interesting columns.
  chain(  interesting.columns
        , trials[.]
        , {for (i in staircased) {.[i] <- factor("varied")}; .}
        , unique
        , .[do.call(order, .),,drop=FALSE]
        ) -> conditions
  factor.data.frame(conditions)
}

factor.data.frame <- function(data) {
  ##Search for "factors" in a data frame...
  index <- colwise(function(X) match(X,X))(data)

  factors = list()
  #start with single column factorings, then go from there
  while (ncol(index) >= 2) {
    #scan until we find a factor, then remove it.
    foundAFactor <- FALSE;
    for (atatime in 1:floor(ncol(index)/2)) {
      for (comb in alply(combn(colnames(index), atatime), 2)) {
        subsets <- dlply(index, comb, function(d) d[setdiff(sort(colnames(d)), comb)])
        if (isTRUE(all.equal(match(subsets, subsets), rep(1, length(subsets))))) {
                                        #we found a factoring for each 
          factors <- c(factors, list(unique(index[comb])))
          foundAFactor <- TRUE
          index <- index[setdiff(colnames(index), comb)]
          break
        }
      }
      if (foundAFactor) break
    }
    if(!foundAFactor) break
  }
  factors <- c(factors, list(unique(index)))

  #convert index back into data...
  lapply(factors,
         function(x) do.call(data.frame,
                             structure(lapply(colnames(x),
                                              function(col) data[x[[col]], col]),
                                       names=colnames(x))))        
}

#Produce a sort of "fully condensed representation" of a data
#frame. Useful for backing out what exactly I was varying in any
#experiment run...
deflate.data.frame <- function(data, order = colnames(data)
                                , ignored = NULL
                                ) {
  #note that set operations generally follow the order of the first argument when they can.
  column.names <- intersect(order, colnames(data))
  if (!is.null(ignored))
    column.names <- setdiff(column.names, ignored)

  data <- data[column.names]

  #data$<<FIELDNAME>> == data$FIELDNAME[data.matches$<<FIELDNAME>>]
  data.matches <- colwise(function(x) match(x,x))(data)

  #data$<<FIELDNAME>> == values<<FIELDNAME>>[data.matches<<FIELDNAME>>]
  values <- mapply(function(m, d) d[unique(m)], data.matches, data)
  data.matches <- colwise(function(x)as.numeric(factor(x)))(data.matches)
  
  #collapse covarying columns, irrespective of lookup values.
  column.matches <- match(data.matches,data.matches)
  column.matches <- column.names[column.matches]
  names(column.matches) <- column.names
  min.columns <- column.matches[!duplicated(column.matches)]
  data.matches <- data.matches[,min.columns]
  list(data=data.matches, columns=column.matches, values=values)
}

##analagous to python zip
zip <- mkchain(sapply(identity), apply(1, identity))

what.fields.are.set.by.randomizer <- function(x) {
  if ("subs.type" %in% names(x)) {
    x$subs <- list(list(type=x$subs.type, subs=x$subs.subs))
  }
  if (length(x$subs) == 0) {
    x$subs <- list()
  }
  chain(x, .$subs
        , lapply(unlist)
        , sapply(splat(str_c))
        , as.character
        , .[!(.=="")]
        , if(length(.) > 0) str_c("trial",.) else .
        )
}

##This interprets the experiment data structure directly to see what fields are
##set up to change
what.varies <- function(runs) {
  chain(  runs$beforeRun.trials.randomizers
          , sapply(sapply, what.fields.are.set.by.randomizer)
          , Reduce(union, .) )
}

#inspect to see if the value asigned is a function or summat...
what.fields.are.staircased.by.randomizer <- function(randomizer) {
  names <-  what.fields.are.set.by.randomizer(randomizer)
  if (length(names) == 0) return(character(0))

  staircased <- chain( randomizer
                      , .$values
                      , lapply(  lapply
                               , function(x)
                                 all(is.na(x)) || "version__.function" %in% names(x)
                               )
                      , lapply(unlist)
                      , Reduce(`|`, .)
                      )

  names[staircased]
}

what.staircases <- function(runs) {
  chain(runs$beforeRun.trials.randomizers
        , sapply(lapply, what.fields.are.staircased.by.randomizer)
        , Reduce(union, .)
        )
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("do.describe", as.list(args))
}
