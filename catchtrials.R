###This explores some of the "catch trials" in each of the variation cases.

fit.model <- function() 0; #to get one in the namespace for the sourced files...
source("common.manipulations.r")
source("modeling.manipulations.R")

##we run one of these for each "variation pool"
my.args <- c("pools/SF.Rdata", "pools/SF.catchtrials.products")

main <- function(...) {
  my.args <- list(...)
  
  ##open output files: first a file to list the output files
  prodfile <- file(my.args[[2]], open='w')

  tryCatch(finally=close(prodfile), {
    #open a pdf file to contain the plots produced
    pdf(file=(pdffile <- sub('\\..*$', '.pdf',my.args[[2]])), onefile=TRUE)
    writeLines(pdffile, prodfile)
    tryCatch(finally=dev.off(), {

      library(plyr)
      library(ggplot2)
      library(psyphy)
      library(boot)
      library(utils)
      library(arm)
      
      load(my.args[[1]])

      common.manipulations(environment())
      common.manipulations.variations(environment())

      pCorrect <-
        ddply(subset(trials, responseInWondow & !is.na(responseTime)),
              c('subject', 'trial.motion.process.radius', 'motionCondition',
                'log.target.spacing', condition.columns),
              function(x) c(n = nrow(x), pCorrect = mean(x$correct)))

      conditionCorrect <-
        ddply(subset(trials, responseInWindow & !is.na(responseTime)),
              c('subject', 'trial.motion.process.radius',
                'motionCondition', condition.columns),
              function(x) c(n = nrow(x), pCorrect = mean(x$correct)))
             
      ##let's see the raw data on accuracy from each subject. We are
      ##looking for eccentricities and feature sizes where the
      ##discrimination performance is not very good. In particular, we
      ##might be interested in situations where local information does
      ##not help you. Let's look at spatial frequency...
      no.spacing.plot <- (ggplot(subset(conditionCorrect, motionCondition != 'incongruent'),
                                aes(pCorrect,
                                    trial.extra.wavelengthScalar * trial.motion.process.radius,
                                    color=motionCondition))
                          + geom_line()
                          )
      print(no.spacing.plot)
      
    })

    
  })
  ##save the data thus produced.
  save(list=ls(), file=(savefile <- sub('\\..*$', '_variations.Rdata', my.args[[2]])))
  writeLines(savefile, prodfile)

}
