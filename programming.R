###Useful R functions for programming, (encouraging terseness.) "Ptools"
library(arm)

mkmatch <- function(index, names) {
 if (is.character(index) || is.factor(index)) {
   matched <- pmatch(index, names, duplicates.ok = TRUE)
   if (any(is.na(matched) & !is.na(index))) {
     stop("Unknown indices")
   }
 } else {
   matched <- index
 }
   matched
}

pad.missing.cells <- function(data, factors) {
  ##add a row containing NA for each combination of factors that is not represented.
  ##This is a workaround for a bug in current reshape / ggplot.
  chain( factors
       , combinations <- llply(., function(x)unique(data[,x,drop=FALSE]))
       , llply(nrow)
       , llply(seq)
       , do.call(expand.grid, .)
       , mapply(function(x, y) x[y,,drop=FALSE], combinations, .)
       , do.call(cbind,.)
       , merge(data, all.x=TRUE)
       )
}

mutate.where <- function(x, subset, ...) {
  ##a combination  of mutate and subset.
  ##mutate those rows where subset evaluates to true, returning the entire modified data frame.
  e <- substitute(subset)
  r <- eval(e, x, parent.frame())
  if (!is.logical(r))
    stop("'subset' must evaluate to logical")
  r <- r & !is.na(r)
  cols <- as.list(substitute(list(...)))[-1]
  cols <- cols[names(cols) != ""]
  .data <- x[r,]
  for (col in names(cols)) {
    .data[[col]] <- eval(cols[[col]], .data, parent.frame())
  }
  for (col in names(cols)) {
    x[r,col] <- .data[,col]
  }
  x
}

keep.if <- function(x, expr, enclos=parent.frame()) {
  ##keep a subset if the expression evaluates to true. Use with ddply.
  if (eval(substitute(expr),x, enclos))
    x
  else
    x[c(),, drop=FALSE]
}

#fake out command line scripts for debugging...
run.command <- function(command) {
  blargs <- strsplit(command, ' ')[[1]]
  trace(commandArgs, at=3, tracer=substitute(args <-  c("--slave","--args", blargs[-1]), list(blargs=blargs)))
  on.exit(untrace(commandArgs))
  source(blargs[[1]])
}

#`subset<-(test,symbol=`.`)
##A shortcut for various assignments of the form:
##complicated.subset[with.long,names=TRUE] <- some.function.of(complicated.subset[with.long,names=TRUE],with.other.options=TRUE)
`%<-%` <- function(target, val) {
  val <- eval(substitute(substitute(val, list(.=quote(target)))))
  eval.parent(substitute(target <- val))
}

prefixing.assign <- function(prefix='', l=list(), env=parent.frame()) {
  for (n in names(l)) {
    assign(paste(prefix,n,sep=""),eval(substitute(l$n,list(n=n))),envir=env)
  }
}

almost.unique <- function(values, thresh = 0.0001) {
  values <- sort(values, na.last=TRUE)
  index <- pipe(values, diff, . > thresh, cumsum, c(0,.))
  tapply(values, index, mean)
}

cluster.to.unique <- function(values, thresh=0.0001) {
  pipe(values,
       .[ord <- order(.)],
       diff, .>thresh, cumsum, c(0,.),
       tapply(values[ord], ., function(x) {x[] <- mean(x); x}),
       unlist,
       .[inverse.permutation(ord)])
}

inverse.permutation <- function(perm) {
  ##if X is a vector expressing a permutation, for example the output
  ##of ORDER(), returns the inverse of that permutation.
  perm[perm] <- 1:length(perm)
  return(perm)
}

## As we can never remember how to use "substitute" on a non-quoted expression.
## I don't think this use of do.call is officially supported but it seems to work.
substitute.nq <- function(expr,...) {
  do.call(substitute, list(expr,...), envir=parent.frame())
}

##this slightly ugly hack is required by the current way that pipe works.
remove.returning <- function(sym) {
  q <- substitute(sym)
  val <- force(sym)
  rm(list=as.character(q), envir=parent.frame())
  val
}

load.as.list <- function(...) {
  a = environment()
  load(envir=a, ...)
  as.list(a)
}

## "fn" is a slight shorthand for defining functions of one argument,
## useful in the arguments to higher-order functions.  Rather than
## function(.) .+1 we have fn(.+1) and otherwise the same.
fn <- function(expr) {
  ff <- eval.parent(substitute(function(.) e, list(e = substitute(expr))))
  attr(ff, "source") <- NULL
  ff
  ##need to do something with printing and use.source?
  ##mm <- match.call()
  ##mm$expr <- NULL
  ##mm[[1]] <- as.name("fn")
  ##attr(ff, "source") <- deparse(mm)
}
