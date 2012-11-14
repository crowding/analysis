#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  source("db_functions.R")
  source("programming.R")
  library(ptools)
  library("plyr")
})

dbfile <- "database.sqlite"

## Shove each file into the SQLite database named in the first arg.
## do.process("database.sqlite", "common/pbm-2012-01-23__14-49-12-ConcentricDirectionDiscriminabilityCritDistance.RData")
## do.process("database.sqlite", "common/nj-2012-03-02__12-54-47-ConcentricDirectionDiscriminabilityCritDistance.RData")

do.process <- function(dbfile, ...) {
  ##open a database connection
  drv <- SQLite()
  infiles <- as.character(list(...))
  if (length(infiles) > 1) {
    print.filename <- TRUE
  } else {
    print.filename <- FALSE
  }

  with.db.connection(drv, dbfile, fn=function(conn) {
    l_ply(infiles, importFile, conn, print.filename)
  })
}

importFile <- function(filename, conn, print.filename) {
  #load the file into an env...
  if (print.filename) print(filename)
  env = new.env()
  load(filename, envir=env)
  for (n in ls(env)) {
    env[[n]] <- kill.list.cols(env[[n]])
    if (nrow(env[[n]]) > 0) {
      env[[n]]$loaded.from <- filename
    }
  }

  with.db.transaction(conn, function() {
    update.database.structure(conn, env)
    insert.data(conn, env)
  })
}

kill.list.cols <- function(df) {
  chain(  df
       , lapply(mode)
       , .[.=="list"]
       , names
       , setdiff(colnames(df),.)
       , df[,.]
       )
}

with.db.transaction <- local({
  #just a quasi-unique (within the transaction) identifier
  savepoint.counter <- 0

  function(conn, fn) {
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
    tryCatch({fn(); completed <- TRUE},
              finally = if (completed) commit() else rollback())
  }
})

send.command <- function(conn, command){
#  cat(command, "\n")
#  cat(substr(command, 1, 75), "\n")
  res <- dbSendQuery(conn, command)
  on.exit(dbClearResult(res), add=TRUE)
  #print(c(completed=dbHasCompleted(res), rowsAffected=dbGetRowsAffected(res)))
}

send.prepared.command <- function(conn, command, ...){
#  cat(command, "\n")
#  cat(substr(command, 1, 70), "\n")
  res <- dbSendPreparedQuery(conn, command, ...)
  on.exit(dbClearResult(res), add=TRUE)
  #print(c(completed=dbHasCompleted(res), rowsAffected=dbGetRowsAffected(res)))
}

update.database.structure <- function(conn, env) {
  ##get descriptions of all existing and putative columns.
  dataset.structure <- get.dataset.structure(conn,env)
  database.structure <- get.database.structure(conn)

  merged.structure <- merge.dataset.and.database.structures(conn, dataset.structure, database.structure)

  create.missing.tables(conn, merged.structure)
  add.missing.fields(conn, merged.structure)
}

insert.data <- function(conn, env) {
  delete.existing.data(conn, env)
  insert.this.data(conn, env)
}

delete.existing.data <- function(conn, env) {
  ##drop records that led from this source file(s).
  tables <- dbListTables(conn)
  source.files <- unique(env$runs$loaded.from)

  for (t in tables) {
    send.prepared.command(conn, sprintf("delete from \"%s\" where loaded_from in (?)", t), data.frame(file=source.files))
  }
}

insert.this.data <- function(conn, env) {
  ##ensure unique id's
  env <- offset.indices(conn,env)
  ##insert the data
  for (i in ls(env)) {
    do.insert(conn,make.db.names(conn, i), env[[i]])
  }
}

offset.indices <- function(conn, env) {
  tables <- dbListTables(conn)
  ##offset our data frame indices to avoid collisions.
  offset <- c()
  for (name in intersect(tables, make.db.names(conn, ls(env)))) {
    if (! "i" %in% colnames(env[[name]])) next

    the.max <- dbGetQuery( conn, sprintf("SELECT max(i) from %s", name))[[1]]
    if (length(the.max) > 0 && all(!is.na(the.max))) {
      offset[[name]] <-
        (  the.max
         - min(env[[name]]$i)
         + 1 )
    } else {
      offset[[name]] <- 0
    }
  }
  #print(offset)
  for (table in names(offset)) {
    for (frame in ls(env)) {
      if (make.db.names(conn, frame) == table) {
        ##same table
        #print(c(frame, "i", offset[[table]]))
        env[[frame]]$i <- env[[frame]]$i + offset[[table]]
      } else {
        ##different table.
        ixcol <- paste(table,sep=".","i")
        if (ixcol %in% names(env[[frame]])) {
          #print(c(frame, ixcol, offset[[table]]))
          env[[frame]][[ixcol]] <- env[[frame]][[ixcol]] + offset[[table]]
        }
      }
    }
  }

  env
}

get.dataset.structure <- function(conn, env) {
  ##report all the columns in the dataset, in the form of a data frame
  ##with at least the following columns:
  ##
  ##"frame" the name of the data frame
  ##"table" same name translated into sql-compatible
  ##"column" the name of the column in the data frame
  ##"field" the name of the field
  ##"mode" the storage mode, either "numeric" or "character" (factors are character)
  ##"is.primary.key" if it's a key
  ##"is.external.key" it it's an external key
  ##"frame.referenced" if it's been referenced
  ##
  ##By convention, a column named"i" corresponds to a primary key column and "*.i" corresponds
  ##to an external key column.

  chain(  data.frame(frame=ls(env), stringsAsFactors=FALSE)
        , mutate( table = make.db.names(conn, frame) )
        , adply(1, function(row)
                cbind(  column=colnames(env[[row$frame]])
                      , mode=vapply(env[[row$frame]]
                          , function(x) if (is.factor(x)) "character" else mode(x)
                          , "")
                      , row[rep(1,length(env[[row$frame]])),])
                )
        , mutate(  is.primary.key = (column == "i")
                 , is.external.key = grepl(".*\\.i$", column)
                 , is.key = is.primary.key | is.external.key
                )
        , mutate.where(.,  is.external.key
                       , frame.referenced = sub("\\.i$", "", column)
                       , column.referenced = "i"
                       , is.external.key = ifelse(frame.referenced %in% .$frame, TRUE, FALSE)
                       )
        )
}

get.database.structure <- function(conn) {
  ##report columns and tables, as a data frame with at least columns:
  ##
  ##"table" the table name.
  ##"field" the field name.
  chain(  quickdf(list(table=dbListTables(conn)))
        , mutate(field=table)
        ##hack to pass through zero rows...
        , rbind(.[c(),,drop=FALSE]
                ,adply(., 1, function(row) data.frame(field=dbListFields(conn, as.character(row$table))
                                            , as.list(row))))
        )
}

merge.dataset.and.database.structures <- function(conn, dataset.structure, database.structure) {
  chain(dataset.structure
        ##hack to pass through zero rows.
        , mutate(  exists.in.dataset = TRUE | as.logical(column) )
        , ddply("table"
                , function(df) mutate(df, field=make.db.names(conn, as.character(column)))
                )
        ) -> dataset.structure

  mutate(database.structure
         , exists.in.database=TRUE | as.logical(field)) -> database.structure

  merged <- merge(dataset.structure, database.structure, by=c("table", "field"), all=TRUE)

  ##compute "field.referenced"
  columns.referencing <-
    subset(merged, is.external.key,
           select=c("frame", "column", "frame.referenced", "column.referenced"))

  columns.referenced <-
    chain(merged
          , subset(select=c("frame", "field", "table", "column"))
          , rename(c("field"="field.referenced", "table"="table.referenced"))
          )

  columns.with.references <-
    merge( columns.referencing, columns.referenced
         , by.x=c("frame.referenced", "column.referenced")
         , by.y=c("frame", "column")
         , all.x=TRUE
         )

  merged <- chain(  columns.with.references
                  , merge(merged, all.y=T)
                  )

  #compute SQL storage type
  chain(  merged
       , mutate(type=ifelse(is.key, "INTEGER", c(character="TEXT", numeric="REAL")[mode])
                , constraint=""
                )
       , mutate.where(is.external.key, constraint=paste("REFERENCES \""
                                         , table.referenced
                                         , "\"(\""
                                         , field.referenced
                                         , "\")"
                                         , sep=""))
        , mutate.where(is.primary.key, constraint="PRIMARY KEY")
        ) -> merged
  merged
}

create.missing.tables <- function(conn, structure) {
  chain(structure
        , ddply("table"
              , keep.if, all(!exists.in.database | is.na(exists.in.database)))
        , daply("table", function(tab) {
          with(tab, paste("\"", field, "\" "
                          , type, " ", constraint
                          , sep="", collapse = ",\n")) -> fields
          with(tab, c(  paste("CREATE TABLE \"" , unique(table) , "\" (" , fields, "); "
                              , sep="", collapse = "")
                      , paste("CREATE INDEX IF NOT EXISTS \""
                              , unique(table), "_loaded_from\" on \""
                              , unique(table), "\" (loaded_from)"
                              , sep="", collapse = "")))
        })) -> maketables
  for (s in c(maketables)){
    cat(substr(s, 1, 75), "\n")
    send.command(conn, s)
  }
}

add.missing.fields <- function(conn, structure) {
  chain(structure
        , rbind(  .[c(),,drop=TRUE]
                , ddply(., "table"
                        , keep.if, !all(!exists.in.database | is.na(exists.in.database))))
        , subset(exists.in.dataset & (is.na(exists.in.database) | !exists.in.database))
        ) -> to.add
  if (nrow( to.add) > 0) {
    statements <-
      (with(to.add, paste(  "ALTER TABLE \"" , table , "\" ADD COLUMN \""
                          , field , "\" " , type , " " , constraint , ";" , sep="")))
    for (s in statements) {
      cat(substr(s, 1, 75), "\n")
      send.command(conn, s)
    }
  }
}


if ("--slave" %in% commandArgs()) {
  args <- commandArgs(trailingOnly=TRUE)
  do.call("do.process", as.list(args))
}
