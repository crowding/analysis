suppressPackageStartupMessages({
  library(RSQLite)
  library(ptools)
})

with.db.connection <- function(fn, ...) {
  conn <- dbConnect(...)
  tryCatch(fn(conn), finally = (dbDisconnect(conn) || stop(dbGetException(conn))))
}

with.db.transaction <- local({
  #just a quasi-unique (within the transaction) identifier
  savepoint.counter <- 0

  function(conn, fn, ...) {
    ##handle sqlite's weird distinction between
    ##transactions and savepoints
    if (dbBeginTransaction(conn)) {
      rollback <- function()dbRollback(conn)
      commit <- function()dbCommit(conn)
    } else {
      ##use a savepoint instead
      savepoint.name <- paste("dsfargeg_savepoint_", savepoint.counter);
      savepoint.counter <<- savepoint.counter + 1
      send.prepared.command(conn, "SAVEPOINT ?", savepoint.name)
      rollback <- function() {
        send.prepared.command(conn, "ROLLBACK TO ?", savepoint.name)
      }
      commit <- function() {
        send.prepared.command(conn, "RELEASE SAVEPOINT ?", savepoint.name)
      }
    }

    ##not using error handlers, just finally. error handlers screw
    ##with the stack trace.
    completed <- FALSE
    tryCatch({result <- fn(...); completed <- TRUE; result},
              finally = if (completed) commit() else rollback())
  }
})

do.insert <- function (conn, name, frame
                       , table=make.db.names(conn,name)
                       , fields=make.db.names(conn, colnames(frame))) {
  if (nrow(frame) < 1) return()
  colnames(frame) <- fields
  into <- paste("\"", fields, "\"", sep="", collapse = ", \n")
  values <- paste(":", fields,  sep="", collapse = ", \n")
  statement <- sprintf("INSERT INTO %s (%s) VALUES ( %s );", table, into, values)
  #cat(substr(statement, 1, 75), "\n")
  res <- dbSendPreparedQuery(conn, statement, frame)
  #print(c(completed=dbHasCompleted(res), rowsAffected=dbGetRowsAffected(res)))
  dbClearResult(res)
}

columns.to.pull <- c("trial.motion.process.radius",
                     "trial.extra.side",
                     "trial.extra.nVisibleTargets",
                     "abs.displacement",
                     "abs.localDirectionContrast",
                     "trial.extra.tf",
                     "trial.extra.wavelengthScalar",
                     "trial.extra.dt",
                     "trial.extra.widthScalar",
                     "trial.extra.durationScalar",
                     "trial.extra.nTargets",
                     "trial.motion.process.order",
                     "trial.motion.process.n",
                     "trial.extra.content_cw",
                     "trial.extra.content_ccw",
                     "visibilityCondition",
                     "folded.localDirectionContrast",
                     "folded.content.with",
                     "folded.content.against",
                     "folded.displacement",
                     "folded.response",
                     "abs.response",
                     "target.spacing",
                     "responseInWindow",
                     "responseTime",
                     "maxResponseTime",
                     "loaded.from",
                     "runs.i",
                     "trial.i",
                     "subject"
                     )

pull.from.sqlite <- function(dbfile, ...) {
  drv <- SQLite()
  trials <- with.db.connection(drv, dbfile, fn=function(conn) {
    pull.from.db(conn, ...)
  })
}

pull.from.db <- function(conn, condition, columns=columns.to.pull, table="trials") {
  #construct a query for pulling the trials nominated by an inner join
  #with the privided data frame.
  column_text <- chain(  columns
                       , make.db.names(conn,.)
                       , paste("\"", ., "\"", sep="", collapse=", "))

  table_text <- chain( table, make.db.names(conn,.), paste("\"", ., "\"", sep="") )

  joiner_name = "temp.joiner"

  query <- sprintf("SELECT %s FROM %s NATURAL JOIN %s;", column_text, table_text, joiner_name)  
  with.db.transaction(conn, function() {
    dbGetQuery(conn, sprintf("DROP TABLE IF EXISTS %s;", joiner_name))
    dbGetQuery(conn, dbBuildTableDefinition(conn, joiner_name, condition, row.names=FALSE))
    do.insert(conn, table=joiner_name, frame=condition)
    dbGetQuery(conn, query)
  })
}
