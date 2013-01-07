is.good.session <- function(desc) {
  ##For the contrast series, nominate files that have at least 2
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

  #count sessions that at have least three contrasts, force two spacings and one element elsewhere.
  #
  lengths <- sapply(desc, nrow)
  valid <- (
    any(contrast)
    && all(  (contrast & lengths >= 2)
           | spacing
           | lengths == 1 )
    )
}
