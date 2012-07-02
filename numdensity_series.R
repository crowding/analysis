is.good.session <- function(desc) {
  ## select all number/density experiments....
  ## and for each experiment, select a session that probed at that contrast....

  ##select number/density experiments. Those are those where nVisibleTargets is set?

  side <- sapply(  desc
                 , mkchain(  colnames
                           , all.equal("trial.extra.side")
                           , isTRUE))

  nTargets <- sapply(  desc
                     , mkchain(  colnames
                               , all(c(  "trial.extra.nTargets"
                                       , "trial.extra.nVisibleTargets") %in% .)
                               , isTRUE))

  #contrast and global speed is allowed to vary.

  global <- sapply(  desc
                  , mkchain(  colnames
                            , all.equal("trial.extra.globalVScalar")
                            , isTRUE
                            ))

  contrast <- sapply(  desc
                     , mkchain(  colnames
                               , "folded.localDirectionContrast" %in% .))

  chain(desc, sapply(nrow), side | contrast | global | nTargets | .==1, all)
}
