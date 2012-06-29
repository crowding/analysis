is.good.session <- function(desc) {
  ##For the spacing series plot, nominate sessions that have at least
  ##4 elements in nTargets, and only 5 elements in nTargets.

  ##For the contrast series, nominate fiels that have at least 2
  ##elements in folded.localDirectionContrast, any number in in
  ##trial.extra.nTargets, and one element in
  contrast <- sapply(desc,
                     mkchain(colnames, all.equal("folded.localDirectionContrast"), isTRUE))
  
  spacing <- sapply(desc, mkchain(colnames, all.equal("trial.extra.nTargets"), isTRUE))
  browser()
  chain(desc, sapply(nrow), {(contrast & .>=2) | spacing}, all)
}
