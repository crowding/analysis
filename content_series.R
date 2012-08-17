is.good.session <- function(desc) {
  ##For the contrast series, nominate fiels that have at least 2
  ##elements in folded.localDirectionContrast, any number in in
  ##trial.extra.nTargets, and one element in everything else.
  contrast <- sapply(  desc
                     , mkchain(  colnames
                               , all.equal("folded.localDirectionContrast")
                               , isTRUE))

  spacing <- sapply(  desc
                    , mkchain(  colnames
                              , all.equal("trial.extra.nTargets")
                              , isTRUE))

  valid <- chain(desc, sapply(nrow), (contrast & .>3) | spacing | .==1, all, . && any(contrast))
}
