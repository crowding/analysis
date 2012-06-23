is.good.session <- function(desc) {
  ##For the spacing series plot, nominate sessions that have at least
  ##4 elements in nTargets, and only 5 elements in nTargets.
  spacing <- sapply(desc, mkchain(colnames, all.equal("trial.extra.nTargets"), isTRUE))
  chain(desc, sapply(nrow), .<2, `|`(spacing), all)
}
