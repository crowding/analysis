#!/bin/env Rscript

replace_extension <- function(filename, new_extension) {
  sub(  "((.)\\.[^.]*|)$"
      , paste("\\2.", new_extension, sep="")
      , filename)
}

##' Inject some named arguments into several calls and evaluate those calls.
##'
##' For a simple example, the call
##' with_args(a="something", c(b="something"), c(c="something else"), .collect=list)
##'
##' is equivalent to writing
##' list(c(a="repeated argument",b=2), c(a="repeated argument",c=3))
##'
##' so that with_args handles the job of distributing the repeated 'a'
##' argument. This can save some typing in some situations, like
##' heavily layered ggplot constructions.
##'
##' We try to interpret each subcall according to R argument matching
##' rules. This might run into problems with generic functions.
##' 
##' @title Inject repeated arguments into several calls.
##' @param ... Named arguments are interpreted as arguments to
##' inject. Unnamed arguments are interpreted as 
##' @param .collect Which function to use to collect all the
##' subcalls. Default is `list'.
##' @param .envir The environment to evaluate in. Defaults to the environment that evaluated
##' @param .override Whether to override arguments that appear to conflict.
##' @return The result of the evaluated call.
##' @author Peter Meilstrup
with_arg <- function(..., .collect=list, .envir=parent.frame(), .override=FALSE)  {
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

##' run_as_command Interpret command line arguments and invokes some
##' function with them.
##'
##' The idea is that to write a command line utility with R, you just
##' write a main() function use Rscript as your hashbang interpreter,
##' and at the end of your R script call run_as_command.
##'
##' TODO: Named arguments given with two dashes, GNU style, will be
##' translated into named arguments passed to the function. A bare
##' double dash means to discontinue named-argument parsing for the
##' rest of the command line.
##'
##' TODO: A help argument will be constructed according to the Roxygen
##' documentation for the function.
##' 
##' @param func Which function to invoke. Defaults to whatever "main"
##' function is defined in the calling scope.
##' @param arguments The command line arguments to parse. By default, uses 
##' @param require_toplevel Only run if invoked from the top level, as
##' from Rscript.
##' @param require_noninteractive Only run if in a non-interactive R
##' session.
##' @return Nothing. Things printed will naturally go out stdout and
##' errors during execution will naturally result in a nonzero exit
##' code.
##' @author Peter Meilstrup
run_as_command <- function(  func=parent.frame()$main
                , arguments=commandArgs(trailingOnly=TRUE)
                , require_toplevel=TRUE, parse_args=TRUE, verify_args=TRUE) {
  if (     (length(sys.frames()) == 1 || !require_toplevel )
        && (!interactive() || !require_noninteractive) ) {
    do.call(func, as.list(arguments))
  }
}
