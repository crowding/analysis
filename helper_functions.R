replace_extension <- function(filename, new_extension) {
  sub(  "((.)\\.[^.]*|)$"
      , paste("\\2.", new_extension, sep="")
      , filename)
}

with_arg <- function(..., .collect=list, .envir=parent.frame(), .override=FALSE)  {
  #substitue arguments into a list of function calls, evaluating them
  #and returning the results as though list() were called.

  ##Any named arguments are interpreted as the arguments to insert,
  ##any unnamed arguments are interpreted as the calls to run. The
  ##arguments are drawn together and collected by a call to the
  ##function named in 'collect.'

  #Intended to be useful in factoring some of the repetition out of
  #complicated ggplot calls.  The bindings are also unevaluated.
  #Ths whole shebang can be organized with Reduce(`+`, list(...))

  ##Note that you ought to be able to nest the calls to pass arguments
  ##down the tree.

  dots <- as.list(substitute(quote(...)))[-1]
  calls <- dots[names(dots) == '']
  args <- dots[names(dots) != '']

  rebuiltCalls <- list()
  for (theCall in calls) {
    theFun <- match.fun(as.list(theCall)[[1]])
    if (!is.primitive(theFun)) {
      theCall <- match.call(theFun, theCall)
    }
    theCall <- as.list(theCall)
    if (.override) {
      theCall[names(args)] = args
    } else {
      safe.args <- setdiff(names(args), names(call))
      theCall[safe.args] = args[safe.args]
    }
    rebuiltCalls <- c(rebuiltCalls, as.call(theCall))
  }
  theCollection <- as.call(c(list(.collect), rebuiltCalls))
  eval(theCollection, envir=.envir)
}
