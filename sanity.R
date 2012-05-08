#sanity functions....
#`[` <- function(x,...,drop=FALSE) .Primitive("[")(x,drop=drop,...)
#`[.data.frame`  gets masked similarly.
# And functions in plyr, if they're available.
#
# and scan through the search path for anything else that defines `[.x`, masking it.

# Disallow sequencing by negative values. 1:0 should retrun the empty sequence.

zip <- function(..., simplify=FALSE)
  mapply(c, simplify=simplify, ...)

#Destructuring bind.

#For loops and such over multiple indexes.

#simple function definitions?
#x %f% x.foo()
#function(x) x.foo()
#%_% foo(_)

#on.exit should default to add=TRUE

##Fix all the subscripting insanity. Out-of-bounds accesses return NA,
##zero subscript returns empty, negative subscripts invert the sense
##of scubscripting, subscripting with single NA returns NA array the
##length of original, too many damn behaviors.

##the functions for constructing a data frame all do way more than
##what you want. strings.as.factors, autonaming, imposing row names, etc.
##data.frame(table=c()) ought to by rights make a frame with one column and no rows!

##even data frame assignment makes factor variables!

##Vector recycling: Replacing zero rows with N colums should just do nothing.

##paste() with empty vectors should produce empty strings. (does stringr do it this way?)

dframe <- function(..., duplicates="error",stringsAsFactors=FALSE, row.names=NULL) {
  ##SENSIBLY create data frames.  No strings as factors. Do the right
  ##thing on zero length input. Option to warn or error on dulicate
  ##column names, instead of silently warning.
  l <- list(...)
  .Names
}

rdbind <- function(..., by.name=TRUE) {
  ## this is basically like "rbind.fill"
  
}

cdbind <- function(..., by.name=FALSE) {
  
}

rabind <- function(..., by.name=FALSE) {

}

cabind <- function(..., by.name=FALSE) {

}

