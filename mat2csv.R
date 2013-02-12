#good lord, reading the help for matlab EXPORT reminded me that matlab
#is horrible at exporting its own data, just as it is horrible at
#importing data! Guess I'll use R to export data from MATLAB.

suppressMessages({
  library(ptools)
  library(R.matlab)
})

main <-function(infile, outfile)
  chain(infile, readMat(fixNames=FALSE), .[[1]][,1,1], as.data.frame,
        write.csv(., outfile))

run_as_command()
