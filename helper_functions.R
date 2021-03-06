library(ptools)

replace_extension <- function(filename, new_extension, append="") {
  sub(  "((.)\\.[^.]*|)$"
      , paste("\\2", append, ".", new_extension, sep="")
      , filename)
}

unique_by <- function(data, columns) {
  dups <- duplicated(data[columns])
  data[!dups,]
}

drop_columns <- function(data, drop) {
  data[colnames(data)[!colnames(data) %in% drop]]
}
