suppressPackageStartupMessages({
	library(ggplot2)
        library(plyr)
        library(ptools)
        library(stringr)
        library(psyphy)
        source("db_functions.R")
        source("graphics_functions.R")
        source("data_functions.R")
})

main <- function(flist, dbfile, outfile) {

  files <- str_trim(readLines(flist))

  trials <- pull.form.sqlite(dbfile, data.frame(loaded_from=files))

}
