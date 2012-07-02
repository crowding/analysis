is.good.session <- function(desc) {
  #look for sessions in which num. targets content with/against are allowed to vary.
  
  contrast <- sapply(  desc
                     , mkchain(  colnames
                               , all(c("folded.content.with", "folded.content.against") %in% .)
                    ))

  spacing <- sapply(  desc
                    , mkchain(  colnames
                              , all.equal("trial.extra.nTargets")
                              , isTRUE))

  any(contrast) && chain(desc, sapply(nrow), contrast | spacing | .==1, all)
  
}
