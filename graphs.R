suppressPackageStartupMessages({
  require(stringr)
})

## this command is run by Make to generate daily plot files.
do.process <- function(infile, outfile, ...) {
  procedures <- as.character(list(...))
  ## Calling convention is: filename in, names of subprocedures.  Each
  ## <filename>.R defines a function of the same name that takes each
  ## element of the file as input, and outputs. Each function also has
  ## a connection open to an file "output" where you ought to print
  ## files that are produced.
  if ("character" %in% class(infile)) {
    data.env <- file.input(infile)
  } else {
    data.env <- infile;
  }
  output = file(outfile, 'w')
  pdf.file <- str_replace(outfile, ".[^.]*$", ".pdf")
  pdf(file=pdf.file, onefile=TRUE, width=10.5, height=8, paper="USr")
  writeLines(pdf.file, outfile)
  on.exit(close(output))
  on.exit(dev.off())

  #open a pdf, too? and write its name to the output?

  for (proc in procedures) {
    ##source the file safely into its own environment, and feed it the
    ##data from our file as arguments. It also gets "infile", "output" and "pdf.file"
    proc.env <- new.env(parent=.GlobalEnv)
    eval(substitute(eval(parse(proc)), list(proc=proc)), envir=proc.env)
    p <- proc.env$process
    do.call(p, c(as.list(data.env), infile, output, pdf.file))
  }
}

file.input <- function(infile) {
  data.env <- new.env(parent=.GlobalEnv)
  load(infile, envir=data.env)
  data.env
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("do.process", as.list(args))
}
