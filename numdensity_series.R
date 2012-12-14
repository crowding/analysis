is.good.session <- function(desc) {
  ## select all number/density experiments....
  ## and for each experiment, select a session that probed at that contrast....

  ##select number/density experiments. Those are those where
  ##nVisibleTargets is varied?

  side <- sapply(  desc
                 , mkchain(  colnames
                           , "trial.extra.side" %in% .
                           ))
  
  nTargets <- sapply(  desc
                     , mkchain(  colnames
                               , all(c(  "trial.extra.nTargets"
                                       , "trial.extra.nVisibleTargets") %in% .)
                               ))

  #contrast and global speed is allowed to vary.
  global <- sapply(  desc
                  , mkchain(  colnames
                            , "trial.extra.globalVScalar" %in% .
                            , isTRUE
                            ))

  contrast <- sapply(  desc
                     , mkchain(  colnames
                               , "folded.localDirectionContrast" %in% .
                               ))

  unvaried <- sapply(desc, nrow) == 1

  bookkeeping <- sapply( desc
                        , mkchain(  colnames
                                  , all(. %in% c("trial.runs.i"))
                                  ))

  #all these factors must be present
  all.required <- list(side, nTargets)
  #only these factors may be present
  only.required <- list(side, global, contrast, nTargets, unvaried, bookkeeping)
  all.requirements <- chain(all.required, sapply(sum), .>=1, all)
  only.requirements <- chain(only.required, Reduce(`|`, .), all)
  all.requirements && only.requirements  
}
