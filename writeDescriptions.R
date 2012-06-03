#tkae the "descriptions" data file and wrote out text files for me too look at.

main <- function(datafile, listfile, directory) {
  oldwidth <- options("width")
  on.exit(options(oldwidth), add=TRUE)
  options(width=10000)
  
  e <- new.env(parent=globalenv())
  load(datafile, envir=e)

  listing <- file(listfile, "w")
  on.exit(close(listing), add=TRUE)

  mapply(e$descriptions, names(e$descriptions), FUN=function(desc, name) {
    fname <- basename(paste(tools:::file_path_sans_ext(name), sep=".", "txt"))
    fout <- file.path(directory, fname)
    
    conn <- file(fout, "w")
    on.exit(close(conn), add=TRUE)
    writeLines(con=conn, capture.output(print(desc)))
    writeLines(con=listing, fout)
  })
  invisible(NULL)
}

if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("main", as.list(args))
}
